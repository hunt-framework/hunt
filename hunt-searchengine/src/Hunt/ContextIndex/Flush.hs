{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
module Hunt.ContextIndex.Flush(
    runFlush
  , FlushPolicy(..)
  ) where

import           Hunt.Common.Document (Document)
import           Hunt.Common.DocId (DocId)
import qualified Hunt.Common.DocIdSet as DocIdSet
import           Hunt.ContextIndex.Types
import           Hunt.ContextIndex.Types.SegmentMap (SegmentId)
import qualified Hunt.ContextIndex.Types.SegmentMap as SegmentMap
import           Hunt.DocTable (DocTable, DValue)
import qualified Hunt.DocTable as DocTable
import           Hunt.ContextIndex.Segment (Docs, Segment (..), Kind(..))
import qualified Hunt.ContextIndex.Segment as Segment
import qualified Hunt.ContextIndex.Documents as Docs
import qualified Hunt.IO.File as IO
import           Hunt.IO.Writer

import           Control.Exception (bracket)
import qualified Data.ByteString.Lazy as LByteString
import           Data.ByteString.Builder (hPutBuilder)
import           Data.ByteString.Builder.Prim ((>*<))
import qualified Data.ByteString.Builder.Prim as Builder
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Binary as Binary
import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary

import           Data.Word
import           System.FilePath
import           System.IO
import qualified Data.Vector.Unboxed as UVector
import qualified Data.Vector.Unboxed.Mutable as UMVector

-- | Runs a `Flush` and writes files to the index directory. This operation is atomic.
runFlush :: (MonadIO m, Binary.Binary (DValue Docs))
         => FlushPolicy
         -> SegmentId
         -> Segment 'Frozen
         -> m (ContextIndex -> ContextIndex)
runFlush policy sid seg = do
  !dix <- writeDocTable policy sid seg
  return $ \ixx ->
    let
      docs :: Docs.DocTable
      docs = Docs.DtIxed (readDocument policy sid) dix
    in ixx { ciSegments = SegmentMap.insertWith (\_ s -> s { segDocs = docs }
                                                ) sid seg (ciSegments ixx)
           }

readDocument :: FlushPolicy -> SegmentId -> Word64 -> Word64 -> IO Document
readDocument policy sid offset size = do
  h <- openFile (fpFlushDirectory policy </> show sid <.> "dt") ReadMode
  hSeek h AbsoluteSeek (fromIntegral offset)
  lbs <- LByteString.hGet h (fromIntegral size)
  let !doc = Binary.runGetOrFail Binary.get lbs
  case doc of
    Left err -> print err >> return undefined
    Right (_, _, doc') -> do doc' `seq` hClose h
                             return doc'

data IndexWriterState a b =
  IWS !Word64 !Int !(UMVector.IOVector (DocId, Word64, Word64)) a b

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
        bufferedAppendWriter :: IO.AppendFile
                             -> Writer IO LByteString.ByteString Word64
        bufferedAppendWriter fp =
          IO.lazyByteStringWriter
          $ IO.bufferedWriter (IO.appendWriter fp) 65536

        -- The work horse. Encode the documents to binary format
        -- and write them.
        indexWriter :: Int
                    -> Writer IO DocId Docs.DocTableIndex
        indexWriter ndocs =
          case bufferedAppendWriter ix of
            W iwStart iwStep iwStop ->
              case bufferedAppendWriter docs of
                W dwStart dwStep dwStop -> W start step stop where

                  start = IWS 0 0 <$> UMVector.unsafeNew ndocs
                                  <*> iwStart
                                  <*> dwStart

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

                    dws' <- dwStep dws docEntry
                    iws' <- iwStep iws dixEntry
                    UMVector.unsafeWrite ix i (did, off, docEntrySize)
                    return $ IWS (off + docEntrySize) (i + 1) ix iws' dws'

                  stop (IWS _ _ ix iws dws) = do
                    _ <- iwStop iws
                    _ <- dwStop dws
                    ix' <- UVector.unsafeFreeze ix
                    return (Docs.DTI ix')

      runWriter (indexWriter numDocs) (DocIdSet.toList docIds)
  where
    dtIxFile = fpFlushDirectory policy </> show sid <.> "dix"
    dtDocFile = fpFlushDirectory policy </> show sid <.> "dt"
