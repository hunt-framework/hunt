{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
module Hunt.ContextIndex.Flush(
    runFlush
  , FlushPolicy(..)
  ) where

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
import           Hunt.DocTable                      (DValue, DocTable)
import qualified Hunt.DocTable                      as DocTable
import qualified Hunt.Index                         as Ix
import qualified Hunt.Index.IndexImpl               as Ix
import qualified Hunt.IO.File                       as IO
import           Hunt.IO.Writer
import           Hunt.Scoring.SearchResult          (searchResultToOccurrences)

import           Control.Arrow                      (second)
import           Control.Exception                  (bracket)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Binary                        as Binary
import qualified Data.Binary.Get                    as Binary
import qualified Data.Binary.Put                    as Binary
import           Data.ByteString.Builder            (Builder)
import           Data.ByteString.Builder            (hPutBuilder)
import qualified Data.ByteString.Builder            as Builder
import           Data.ByteString.Builder.Prim       ((>*<))
import qualified Data.ByteString.Builder.Prim       as Prim
import qualified Data.ByteString.Lazy               as LByteString
import           Data.Foldable
import           Data.IORef
import           Data.Profunctor
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Data.Text.Encoding                 as Text
import           Data.Traversable

import qualified Data.Vector.Unboxed                as UVector
import qualified Data.Vector.Unboxed.Mutable        as UMVector
import           Data.Word
import           System.FilePath
import           System.IO

-- | Runs a `Flush` and writes files to the index directory. This operation is atomic.
runFlush :: (MonadIO m, Binary.Binary (DValue Docs))
         => FlushPolicy
         -> SegmentId
         -> Segment 'Frozen
         -> m (ContextIndex -> ContextIndex)
runFlush policy sid seg = do
  !dix <- writeDocTable policy sid seg
  writeIndex policy sid seg
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

data IndexWriterState a b =
  IWS !Word64 !Int !(UMVector.IOVector (DocId, Word64, Word64)) a b

data TermWriterState a =
  TWS !Word64 !Text a

writeIndex :: (MonadIO m)
           => FlushPolicy
           -> SegmentId
           -> Segment 'Frozen
           -> m ()
writeIndex policy segId seg = liftIO $ do

  let
    -- A writer which writes int as Word64 big endian.
    intWriter :: Writer IO Builder Word64 -> Writer IO Int Word64
    intWriter = lmap (Prim.primFixed Prim.word64BE . fromIntegral)

    -- Convert an occurrence tuple to a builder.
    occWriter :: Writer IO Builder Word64
              -> Writer IO (DocId, Int, Word64) Word64
    occWriter = lmap go
      where go :: (DocId, Int, Word64) -> Builder
            go (did, noccs, off) =
              Prim.primFixed (Prim.word64BE >*< Prim.word64BE >*< Prim.word64BE)
              ( fromIntegral (unDocId did)
              , ( fromIntegral noccs
                , off
                )
              )

    -- Term writer for delta encoded terms.
    termWriter :: Writer IO Builder Word64
               -> Writer IO (Text, Int, Word64) Word64
    termWriter (W wstart wstep wstop) = W start step stop
      where
        start = TWS 0 Text.empty <$> wstart

        step (TWS n lastWord ws) (word, noccs, occOff) = do
          let
            header :: Int -> Int -> Builder
            header a b = Prim.primFixed (Prim.word64BE >*< Prim.word64BE)
              ( fromIntegral a
              , fromIntegral b
              )

            meta :: Builder
            meta = Prim.primFixed (Prim.word64BE >*< Prim.word64BE)
              ( fromIntegral noccs
              , occOff
              )

          case Text.commonPrefixes lastWord word of
            Just (commonPrefix, suffix, suffix') -> do
              ws' <- wstep ws (header (Text.length commonPrefix) (Text.length suffix')
                               `mappend` Builder.byteString (Text.encodeUtf8 suffix')
                               `mappend` meta)
              return $ TWS (n + 1) word ws'
            Nothing -> do
              ws' <- wstep ws (header 0 (Text.length word)
                              `mappend` Builder.byteString (Text.encodeUtf8 word)
                              `mappend` meta)
              return $ TWS (n + 1) word ws'

        stop (TWS _ _ ws) = wstop ws

    indexWriter :: Writer IO Int Word64                  -- position writer
                -> Writer IO (DocId, Int, Word64) Word64 -- occurrence writer
                -> Writer IO (Text, Int, Word64) Word64  -- term writer
                -> Writer IO (Text, Occurrences) Word64
    indexWriter pw ow tw = case ow of
      (W owstart owstep owstop) ->  case tw of
        (W twstart twstep twstop) -> W start step stop where

          start = twstart

          step ts (term, occs) = do
            -- write occurrences
            ow <- owstart
            ow' <- foldlM (\s (did, pos) -> do
                              -- for each document in occurrences
                              -- write their positions
                              posOff <- runWriter pw (Positions.toAscList pos)
                              owstep s (did, Positions.size pos, posOff)
                          ) ow (DocIdMap.toList occs)
            occOff <- owstop ow'
            -- we have stored the occurrences. Write the actual term.
            twstep ts (term, Occurrences.size occs, occOff)

          stop = twstop

    bufferedAppendWriter :: IO.AppendFile
                         -> Int
                         -> IO (Writer IO Builder Word64)
    bufferedAppendWriter fp nSz = do
      offRef <- newIORef 0
      return $ case IO.bufferedWriter (IO.appendWriter fp) nSz of
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
          <$> (intWriter <$> bufferedAppendWriter pos 1024)
          <*> (occWriter <$> bufferedAppendWriter occs 32768)
          <*> (termWriter <$> bufferedAppendWriter terms 65536)

    case iw of
      W istart istep istop ->
        for (Segment.cxMap (segIndex seg)) $ \iximpl -> do
          s <- istart
          istop =<< foldlM istep s (ixToList iximpl)

  return ()
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
        numDocs :: Int
        numDocs = DocIdSet.size docIds

        -- A buffered writer which appends to a file when the buffer
        -- is full. As we use ByteString.Builder they take lazy
        -- bytestrings.
        bufferedAppendWriter :: IO.AppendFile -> Writer IO Builder Word64
        bufferedAppendWriter fp =
          IO.bufferedWriter (IO.appendWriter fp) 65536

        -- The work horse. Encode the documents to binary format
        -- and write them.
        doctableWriter :: Int
                       -> Writer IO DocId Docs.DocTableIndex
        doctableWriter ndocs =
          case bufferedAppendWriter ix of
            W iwstart iwstep iwstop ->
              case bufferedAppendWriter docs of
                W dwstart dwstep dwstop -> W start step stop where

                  -- Initialize writer.
                  start = IWS 0 0 <$> UMVector.unsafeNew ndocs
                                  <*> iwstart
                                  <*> dwstart

                  -- For every DocId do...
                  step (IWS off i ix iws dws) did = do
                    Just doc <- Segment.lookupDocument did seg

                    let
                      docEntry :: LByteString.ByteString
                      docEntry = Binary.runPut (Binary.put doc)

                      docEntrySize :: Word64
                      docEntrySize = fromIntegral $ LByteString.length docEntry

                      dixEntry :: LByteString.ByteString
                      dixEntry = Binary.runPut $ do
                        Binary.putWord64be off
                        Binary.putWord64be docEntrySize

                    dws' <- dwstep dws (Builder.lazyByteString docEntry)
                    iws' <- iwstep iws (Builder.lazyByteString dixEntry)
                    UMVector.unsafeWrite ix i (did, off, docEntrySize)
                    return $ IWS (off + docEntrySize) (i + 1) ix iws' dws'

                  -- Done here
                  stop (IWS _ _ ix iws dws) = do
                    _ <- iwstop iws
                    _ <- dwstop dws
                    ix' <- UVector.unsafeFreeze ix
                    return (Docs.DTI ix')

      runWriter (doctableWriter numDocs) (DocIdSet.toList docIds)
  where
    dtIxFile = fpFlushDirectory policy </> show sid <.> "dx"
    dtDocFile = fpFlushDirectory policy </> show sid <.> "dt"
