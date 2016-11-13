{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RecordWildCards #-}
module Hunt.SegmentIndex.Store.Documents where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocDesc                (FieldValue (..))
import qualified Hunt.Common.DocDesc                as DocDesc
import           Hunt.Common.DocId
import qualified Hunt.Common.DocIdMap               as DocIdMap
import qualified Hunt.Common.DocIdSet               as DocIdSet
import           Hunt.Common.Document
import           Hunt.Common.Occurrences            (Occurrences)
import qualified Hunt.Common.Positions              as Positions
import           Hunt.Index.Schema
import           Hunt.IO.Buffer                     (Buffer, Flush)
import qualified Hunt.IO.Buffer                     as Buffer
import           Hunt.IO.Files
import           Hunt.IO.Write
import           Hunt.Scoring.SearchResult
import           Hunt.SegmentIndex.Store.DirLayout
import           Hunt.SegmentIndex.Types
import           Hunt.SegmentIndex.Types.Generation
import           Hunt.SegmentIndex.Types.SegmentId
import           Hunt.SegmentIndex.Types.SegmentMap (SegmentMap)
import qualified Hunt.SegmentIndex.Types.SegmentMap as SegmentMap
import           Hunt.SegmentIndex.Types.TermInfo

import           Control.Monad.State.Strict
import qualified Data.Binary                        as Binary
import qualified Data.Binary.Put                    as Binary
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as ByteString
import qualified Data.ByteString.Builder            as Builder
import           Data.Foldable
import           Data.Key
import           Data.Map                           (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import qualified Data.Text.Encoding                 as Text
import qualified Data.Text.Foreign                  as Text
import qualified Data.Vector                        as V
import qualified Data.Vector.Unboxed.Mutable        as UM
import           Data.Word                          hiding (Word)
import           Foreign.Ptr
import           System.FilePath
import           System.IO                          (IOMode (WriteMode),
                                                     withFile)

import           Prelude                            hiding (Word, words)

type BytesWritten = Int

type SortedFields = V.Vector Field

writeFieldInfos :: FilePath
                -> SegmentId
                -> SortedFields
                -> IO ()
writeFieldInfos indexDirectory segmentId fields = do
  Binary.encodeFile
    (indexDirectory </> fieldInfoFile segmentId)
    (V.toList fields)

-- | Write a list of 'Document's to disk in an apropriate format
-- for efficient retrieval.
writeDocuments :: FilePath
               -> SegmentId
               -> SortedFields
               -> [Document]
               -> IO ()
writeDocuments ixDir sid fields docs = do

  withAppendFile (ixDir </> fieldDataFile sid) $ \fdtFile ->
    withAppendFile (ixDir </> fieldIndexFile sid) $ \fdxFile -> do

    Buffer.withBuffer (2 * 1024) $ \fdxBuffer -> do
      Buffer.withBuffer (16 * 1024) $ \fdtBuffer -> do

        let
          fdtFlush :: Flush BytesWritten
          fdtFlush = append fdtFile
          {-# NOINLINE fdtFlush #-}

          fdxFlush :: Flush BytesWritten
          fdxFlush = append fdxFile
          {-# NOINLINE fdxFlush #-}

          -- For each field of a document write the fields one
          -- by one, sorted in 'fields' order.
          foldFields descr docBytesWritten fieldRank field = do
            case DocDesc.lookupValue field descr of
              FV_Null -> return docBytesWritten
              value   -> do
                n <- putWrite
                     fdtBuffer
                     fdtFlush
                     (vint >*< fieldValueWrite)
                     (fieldRank, value)
                return (docBytesWritten + n)

          -- For each document write its fields to the
          -- field data buffer
          foldDocs bytesWritten doc = do
            let
              descr = desc doc
              !size = DocDesc.size descr

            lenBytes <- putWrite
                        fdtBuffer
                        fdtFlush
                        vint
                        size

            docBytes <- V.ifoldM' (foldFields descr) 0 fields

            -- write the offset of the document field data
            -- to the document field index.

            _ <- putWrite
                 fdxBuffer
                 fdxFlush
                 word64
                 (fromIntegral bytesWritten)

            return (lenBytes + docBytes + bytesWritten)

        _ <- foldlM foldDocs 0 docs

        Buffer.flush fdtBuffer fdtFlush
        Buffer.flush fdxBuffer fdxFlush
        return ()
  return ()

-- Helper which flushes and retries if buffer
-- is full
putWrite :: Buffer -> Flush a -> Write t -> t -> IO Int
putWrite buffer flush (W size write) a =
  putFlush buffer flush (size a) (write a)
  where putFlush buffer flush size write = do
          -- FIXME: we need to account for cases where
          -- the Write is bigger than overall buffer size.
          -- Resize the buffer etc.
          notFull <- Buffer.hasEnoughBytes buffer size
          case notFull of
            False -> do _ <- Buffer.flush buffer flush
                        return ()
            True -> return ()
          Buffer.put buffer write
{-# INLINE putWrite #-}

vint :: Write Int
vint = fromIntegral >$< varint64
{-# INLINE vint #-}

-- | A 'Write' for 'DocDesc' fields.
fieldValueWrite :: Write FieldValue
fieldValueWrite = W size write
  where
    W word8Size word8Write = word8
    W vintSize vintWrite   = fromIntegral >$< varint64
    W bsSize bsWrite       = bytestring'
    W tSize tWrite         = text

    size (FV_Int i)    = vintSize i + tagSize
    size (FV_Float _f) = undefined
    size (FV_Text s)   = tSize s + tagSize
    size (FV_Binary b) = bsSize b + tagSize
    size (FV_Json j)   = bsSize j + tagSize
    size FV_Null       = 0

    write (FV_Int i) op    = word8Write 0 op >>= vintWrite i
    write (FV_Float _f) op = word8Write 1 op >>= undefined
    write (FV_Text s) op   = word8Write 2 op >>= tWrite s
    write (FV_Binary b) op = word8Write 3 op >>= bsWrite b
    write (FV_Json j) op   = word8Write 4 op >>= bsWrite j
    write FV_Null op       = return op

    tagSize = word8Size 0
{-# INLINE fieldValueWrite #-}
