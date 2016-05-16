{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
module Hunt.ContextIndex.Flush(
    runFlush
  , FlushPolicy(..)
  ) where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId                     (DocId, unDocId)
import qualified Hunt.Common.DocIdMap                  as DocIdMap
import qualified Hunt.Common.DocIdSet                  as DocIdSet
import           Hunt.Common.Document                  (Document)
import           Hunt.Common.Occurrences               (Occurrences)
import qualified Hunt.Common.Occurrences               as Occurrences
import qualified Hunt.Common.Positions                 as Positions
import qualified Hunt.ContextIndex.Documents           as Docs
import           Hunt.ContextIndex.Segment             (Docs, Kind (..),
                                                        Segment (..))
import qualified Hunt.ContextIndex.Segment             as Segment
import           Hunt.ContextIndex.Types
import           Hunt.ContextIndex.Types.SegmentMap    (SegmentId)
import qualified Hunt.ContextIndex.Types.SegmentMap    as SegmentMap
import           Hunt.DocTable                         (DValue)
import qualified Hunt.Index                            as Ix
import qualified Hunt.Index.IndexImpl                  as Ix
import qualified Hunt.IO.File                          as IO
import           Hunt.IO.Writer
import           Hunt.Scoring.SearchResult             (searchResultToOccurrences)

import           Control.Arrow                         (second)
import           Control.Exception                     (bracket)
import           Control.Monad.IO.Class
import qualified Data.Binary                           as Binary
import qualified Data.Binary.Get                       as Binary
import qualified Data.Binary.Put                       as Binary
import           Data.Bits
import           Data.ByteString.Builder               (Builder)
import qualified Data.ByteString.Builder               as Builder
import           Data.ByteString.Builder.Prim          ((>*<))
import qualified Data.ByteString.Builder.Prim          as Prim
import qualified Data.ByteString.Builder.Prim.Internal as Prim
import qualified Data.ByteString.Lazy                  as LByteString
import           Data.Foldable
import           Data.IORef
import           Data.Map                              (Map)
import qualified Data.Map.Strict                       as Map
import           Data.Profunctor
import           Data.Text                             (Text)
import qualified Data.Text                             as Text
import qualified Data.Text.Encoding                    as Text
import           Data.Traversable
import qualified Data.Vector.Unboxed                   as UVector
import qualified Data.Vector.Unboxed.Mutable           as UMVector
import           Data.Word
import           Foreign.Ptr
import           Foreign.Storable
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
    -- A writer which writes int as Word64 big endian.
    intWriter :: Writer IO Builder Word64 -> Writer IO Int Word64
    intWriter = lmap (Prim.primBounded varint . fromIntegral)

    -- Convert an occurrence triple (DocId, number of positions,
    -- offset of positions) to a builder.
    occWriter :: Writer IO Builder Word64
              -> Writer IO (DocId, Int, Word64) Word64
    occWriter = lmap go
      where go :: (DocId, Int, Word64) -> Builder
            go (did, noccs, off) =
              Prim.primBounded (varint >*< varint >*< varint)
              ( fromIntegral (unDocId did)
              , ( fromIntegral noccs
                , off
                )
              )

    -- the actual term writer. Writes the term and the number
    -- of occurrences and their offset.
    -- To save space we only store the suffix compared to the
    -- last added word.
    termWriter :: Writer IO Builder Word64
               -> Writer IO (Text, Int, Word64) Word64
    termWriter (W wstart wstep wstop) = W start step stop
      where
        start = TWS 0 Text.empty <$> wstart

        step (TWS n lastWord ws) (word, noccs, occOff) = do
          let
            prefix, suffix :: Text
            (prefix, suffix) = case Text.commonPrefixes lastWord word of
              Just (pfx, _, sfx) -> (pfx, sfx)
              Nothing            -> (Text.empty, word)

            header :: Int -> Int -> Builder
            header a b = Prim.primBounded (varint >*< varint)
              ( fromIntegral a
              , fromIntegral b
              )

            meta :: Builder
            meta = Prim.primBounded (varint >*< varint)
              ( fromIntegral noccs
              , occOff
              )

          ws' <- wstep ws (header (Text.length prefix) (Text.length suffix)
                          `mappend` Builder.byteString (Text.encodeUtf8 suffix)
                          `mappend` meta)
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
                -> Writer IO (DocId, Int, Word64) Word64 -- occurrence writer
                -> Writer IO (Text, Int, Word64) Word64  -- term writer
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
                              owstep s (did, Positions.size pos, posOff)
                          ) ow (DocIdMap.toList occs)
            occOff <- owstop ow'
            -- we have stored the occurrences. Write the actual term.
            IWS (n + 1) <$> twstep ts (term, Occurrences.size occs, occOff)

          stop (IWS n ts) = (,) <$> pure n <*> twstop ts

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
          <$> (intWriter  <$> bufferedAppendWriter pos 1024)
          <*> (occWriter  <$> bufferedAppendWriter occs 32768)
          <*> (termWriter <$> bufferedAppendWriter terms 65536)

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
                       -> Writer IO Builder Word64
                       -> Writer IO Builder Word64
                       -> Writer IO (DocId, Document) Docs.DocTableIndex
        doctableWriter ndocs iw dw = case iw of
          W iwstart iwstep iwstop -> case dw of
            W dwstart dwstep dwstop -> W start step stop where

              -- Initialize writer.
              start = DTS 0 0 <$> UMVector.unsafeNew ndocs
                              <*> iwstart
                              <*> dwstart

              -- For every DocId do...
              step (DTS off i ix iws dws) (did, doc) = do
                let
                  docEntry :: LByteString.ByteString
                  docEntry = Binary.runPut (Binary.put doc)

                  docEntrySize :: Word64
                  docEntrySize = fromIntegral $ LByteString.length docEntry

                  dixEntry :: Builder
                  dixEntry = Prim.primBounded (varint >*< varint)
                             (off, docEntrySize)

                dws' <- dwstep dws (Builder.lazyByteString docEntry)
                iws' <- iwstep iws dixEntry
                UMVector.unsafeWrite ix i (did, off, docEntrySize)
                return $ DTS (off + docEntrySize) (i + 1) ix iws' dws'

              -- Done here
              stop (DTS _ _ ix iws dws) = do
                _ <- iwstop iws
                _ <- dwstop dws
                ix' <- UVector.unsafeFreeze ix
                return (Docs.DTI ix')

      let dw = doctableWriter
               numDocs
               (bufferedAppendWriter ix)
               (bufferedAppendWriter docs)

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

varint :: Prim.BoundedPrim Word64
varint = Prim.boudedPrim 9 go
  where
    go :: Word64 -> Ptr Word8 -> IO (Ptr Word8)
    go !n !op
      | n < 0x80  = do poke op (fromIntegral n)
                       return (op `plusPtr` 1)
      | otherwise = do poke op (setBit (fromIntegral n) 7)
                       go (n `unsafeShiftR` 7) (op `plusPtr` 1)
{-# INLINE CONLIKE varint #-}
