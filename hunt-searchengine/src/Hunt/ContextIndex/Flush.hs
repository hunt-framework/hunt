{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE RankNTypes                #-}
module Hunt.ContextIndex.Flush(
    runFlush
  , FlushPolicy(..)
  ) where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId                  (DocId, unDocId)
import qualified Hunt.Common.DocIdMap               as DocIdMap
import qualified Hunt.Common.DocIdSet               as DocIdSet
import           Hunt.Common.Document               (Document)
import           Hunt.Common.Occurrences            (Occurrences)
import qualified Hunt.Common.Occurrences            as Occurrences
import qualified Hunt.Common.Positions              as Positions
import qualified Hunt.ContextIndex.Documents        as Docs
import           Hunt.ContextIndex.Segment          (Docs, Kind (..),
                                                     Segment (..))
import qualified Hunt.ContextIndex.Segment          as Segment
import           Hunt.ContextIndex.Types
import           Hunt.ContextIndex.Types.SegmentMap (SegmentId)
import qualified Hunt.ContextIndex.Types.SegmentMap as SegmentMap
import           Hunt.DocTable                      (DValue)
import qualified Hunt.Index                         as Ix
import qualified Hunt.Index.IndexImpl               as Ix
import           Hunt.IO.Encoder
import qualified Hunt.IO.File                       as IO

import           Hunt.Scoring.SearchResult          (searchResultToOccurrences)

import           Control.Arrow                      (second)
import           Control.Exception                  (bracket)
import           Control.Monad.IO.Class
import qualified Data.Binary                        as Binary
import qualified Data.Binary.Get                    as Binary
import qualified Data.Binary.Put                    as Binary
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as ByteString
import qualified Data.ByteString.Lazy               as LByteString
import           Data.Foldable
import           Data.IORef
import           Data.Map                           (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Data.Text.Encoding                 as Text
import           Data.Traversable
import qualified Data.Vector.Unboxed                as UVector
import qualified Data.Vector.Unboxed.Mutable        as UMVector
import           Data.Word
import           System.FilePath

-- | Runs a `Flush` and writes files to the index directory. This operation is atomic.
runFlush :: (MonadIO m, Binary.Binary (DValue Docs))
         => FlushPolicy
         -> SegmentId
         -> Segment 'Frozen
         -> m (ContextIndex -> ContextIndex)
runFlush policy sid seg = do
  -- write the doctable to obtain a DocTableIndex
  !dix <- writeDocTable policy sid seg
  -- write the terms to obtain a SegmentIndex
  -- TODO: investigate if we can now use full on-disk indices.
  !six <- writeIndex policy sid seg

  return $ \ixx ->
    let
      docs :: Docs.DocTable
      docs = Docs.DtIxed (readDocument policy sid) dix
    in ixx { ciSegments = SegmentMap.insertWith (\_ s -> s { segDocs = docs }
                                                ) sid seg (ciSegments ixx)
           }

readDocument :: FlushPolicy -> SegmentId -> Word64 -> Word64 -> IO Document
readDocument policy sid offset size =
  bracket (IO.openRandomAccessFile docTable) IO.closeRandomAccessFile $
    \ram -> do
      IO.seekRandomAccessFile offset ram
      bs <- IO.readRandomAccessFile (fromIntegral size) ram
      let doc = Binary.runGetOrFail Binary.get (LByteString.fromStrict bs)
      case doc of
        Left err -> print err >> return undefined
        Right (_, _, doc') -> return doc'
  where
    docTable = fpFlushDirectory policy </> show sid <.> "dt"

data DocTableWriterState a b =
  DTS !Word64 !Int !(UMVector.IOVector (DocId, Word64, Word64)) a b

data IndexWriterState a =
  IWS !Int !a

data TermWriterState a =
  TWS !Word64 !Text !a

newtype SegmentIndex =
  SegmentIndex { siContexts :: Map Context (Int, Word64) }

writeIndex :: (MonadIO m)
           => FlushPolicy
           -> SegmentId
           -> Segment 'Frozen
           -> m SegmentIndex
writeIndex policy segId seg = liftIO $ do
  let
    posEncoding :: Encoding Int
    posEncoding = fromIntegral >$< varint

    occEncoding :: Encoding (DocId, (Int, Word64))
    occEncoding = (fromIntegral . unDocId >$< varint)
                  >*< (fromIntegral >$< varint)
                  >*< varint

    -- termWriter :: (forall a. Writer IO (Pair (Encoding a) a) Word64)
    --            -> Writer IO (Text, (Int, Word64)) Word64
    termWriter (W wstart wstep wstop) = W start step stop where

          start = TWS 0 Text.empty <$> wstart

          step (TWS n lastWord ws) (word, (noccs, occOff)) = do
            let
              prefix, suffix :: Text
              (prefix, suffix) = case Text.commonPrefixes lastWord word of
                Just (pfx, _, sfx) -> (pfx, sfx)
                Nothing            -> (Text.empty, word)

              prefix', suffix' :: ByteString
              prefix' = Text.encodeUtf8 prefix
              suffix' = Text.encodeUtf8 suffix

              termEncoding :: Encoding (Int
                                       , (Int
                                         , (ByteString
                                           , (Int
                                             , Word64))))
              termEncoding = (fromIntegral >$< varint)
                             >*< (fromIntegral >$< varint)
                             >*< byteString (ByteString.length suffix')
                             >*< (fromIntegral >$< varint)
                             >*< varint

            ws' <- wstep ws (P termEncoding
                             ( ByteString.length prefix'
                             , ( ByteString.length suffix'
                               , ( suffix'
                                 , ( noccs
                                   , occOff
                                   )
                                 )
                               )
                             )
                            )
            return $ TWS (n + 1) word ws'

          stop (TWS _ _ ws) = wstop ws

    -- the actual index writer combining the hierarchies:
    --
    --      terms
    --        |
    --    occurrences
    --        |
    --     positions
    --
    -- Returns the number of terms written and their offset.
    indexWriter :: Writer IO Int Word64                  -- position writer
                -> Writer IO (DocId, (Int, Word64)) Word64 -- occurrence writer
                -> Writer IO (Text, (Int, Word64)) Word64  -- term writer
                -> Writer IO (Text, Occurrences) (Int, Word64)
    indexWriter pw ow tw = case ow of
      (W owstart owstep owstop) ->  case tw of
        (W twstart twstep twstop) -> W start step stop where

          start = IWS 0 <$> twstart

          step (IWS n ts) (term, occs) = do
            -- write occurrences
            ow <- owstart
            ow' <- foldlM (\s (did, pos) -> do
                              -- for each document in occurrences
                              -- write their positions
                              posOff <- runWriter pw (Positions.toAscList pos)
                              owstep s (did, (Positions.size pos, posOff))
                          ) ow (DocIdMap.toList occs)
            occOff <- owstop ow'
            -- we have stored the occurrences. Write the actual term.
            IWS (n + 1) <$> twstep ts (term, (Occurrences.size occs, occOff))

          stop (IWS n ts) = (,) <$> pure n <*> twstop ts

    fixedEncodingWriter :: Int
                        -> Encoding a
                        -> IO.AppendFile
                        -> IO (Writer IO a Word64)
    fixedEncodingWriter bufSz enc fp = do
      offRef <- newIORef 0
      return $ case bufferedEncWriter bufSz enc (appendWriter fp) of
        W wstart wstep wstop -> W wstart wstep stop where
          stop ws = do
            n <- wstop ws
            off <- readIORef offRef
            modifyIORef' offRef (+ n)
            return off

    -- dynEncodingWriter :: forall a. Int
    --                   -> IO.AppendFile
    --                   -> IO (Writer IO (Pair (Encoding a) a) Word64)
    dynEncodingWriter bufSz fp = do
      offRef <- newIORef 0
      return $ case bufferedEncWriter' bufSz (appendWriter fp) of
        W wstart wstep wstop -> W wstart wstep stop where
          stop ws = do
            n <- wstop ws
            off <- readIORef offRef
            modifyIORef' offRef (+ n)
            return off

  bracket (IO.openAppendFile termFile) IO.closeAppendFile $ \terms ->
    bracket (IO.openAppendFile occFile) IO.closeAppendFile $ \occs ->
    bracket (IO.openAppendFile posFile) IO.closeAppendFile $ \pos -> do

    let
      ixToList :: Ix.IndexImpl -> [(Text, Occurrences)]
      ixToList (Ix.IndexImpl ix) =
        second searchResultToOccurrences <$> Ix.toList ix

    iw <- indexWriter
          <$> fixedEncodingWriter 1024 posEncoding pos
          <*> fixedEncodingWriter 32768 occEncoding occs
          <*> (termWriter <$> dynEncodingWriter 65536 terms)

    -- ok, for each context write the index.
    xs <- for (Map.toList (Segment.cxMap (segIndex seg))) $ \(cx, iximpl) -> do
      (nwords, offset) <- runWriter iw (ixToList iximpl)
      -- we have now written an index to disk. Having
      -- the count (nwords) of terms in the dictionary
      -- and the offset at where they are located in the
      -- *.tis file.
      return (cx, (nwords, offset))

    return $ SegmentIndex (Map.fromDistinctAscList xs)
  where
    termFile = fpFlushDirectory policy </> show segId <.> "tis"
    occFile = fpFlushDirectory policy </> show segId <.> "occ"
    posFile = fpFlushDirectory policy </> show segId <.> "pos"

writeDocTable :: (MonadIO m, Binary.Binary (DValue Docs.DocTable))
              => FlushPolicy
              -> SegmentId
              -> Segment 'Frozen
              -> m Docs.DocTableIndex
writeDocTable policy sid seg = liftIO $ do
  -- Don't access DocTable directly, as it could be an already
  -- flushed Segment so we don't clutter memory
  docIds <- Segment.segmentDocIds seg

  bracket (IO.openAppendFile dtIxFile) IO.closeAppendFile $ \ix -> do
    bracket (IO.openAppendFile dtDocFile) IO.closeAppendFile $ \docs -> do

      -- A vector representation for our new new DocTable index.
      -- We use two vectors, one for the strictly ordered DocIds
      -- and one for the (offset, size) info for the disk seek.

      let
        docIndexEncoding :: Encoding (DocId, (Word64, Word64))
        docIndexEncoding = (fromIntegral . unDocId >$< varint)
                           >*< varint
                           >*< varint

        numDocs :: Int
        numDocs = DocIdSet.size docIds

        -- The work horse. Encode the documents to binary format
        -- and write them.
        docTableWriter :: Int
                       -> Writer IO LByteString.ByteString Word64
                       -> Writer IO (DocId, (Word64, Word64)) Word64
                       -> Writer IO (DocId, Document) Docs.DocTableIndex
        docTableWriter ndocs dw iw = case dw of
          W dwstart dwstep dwstop -> case iw of
            W iwstart iwstep iwstop -> W start step stop where

              start = DTS 0 0 <$> UMVector.unsafeNew ndocs
                              <*> dwstart
                              <*> iwstart

              step (DTS off i ix dws iws) (did, doc) = do
                let
                  docEntry :: LByteString.ByteString
                  docEntry = Binary.runPut (Binary.put doc)

                  docEntrySize :: Word64
                  docEntrySize = fromIntegral $ LByteString.length docEntry

                dws' <- dwstep dws docEntry
                iws' <- iwstep iws (did, (fromIntegral docEntrySize, off))
                UMVector.unsafeWrite ix i (did, off, docEntrySize)
                return $ DTS (off + docEntrySize) (i + 1) ix dws' iws'

              stop (DTS _ _ ix dws iws) = do
                _ <- iwstop iws
                _ <- dwstop dws
                ix' <- UVector.unsafeFreeze ix
                return (Docs.DTI ix')

        dw = docTableWriter
             numDocs
             (lazyByteStringWriter (bufferedByteStringWriter 65536 (appendWriter docs)))
             (bufferedEncWriter 65536 docIndexEncoding (appendWriter ix))

      case dw of
        W dwstart dwstep dwstop -> do
          dws0 <- dwstart
          dws' <- foldlM (\dws did -> do
                             Just doc <- Segment.lookupDocument did seg
                             dwstep dws (did, doc)
                         ) dws0 (DocIdSet.toList docIds)
          dwstop dws'
  where
    dtIxFile = fpFlushDirectory policy </> show sid <.> "dx"
    dtDocFile = fpFlushDirectory policy </> show sid <.> "dt"
