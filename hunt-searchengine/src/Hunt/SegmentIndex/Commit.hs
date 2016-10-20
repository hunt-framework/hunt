{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE PatternGuards #-}
module Hunt.SegmentIndex.Commit (
    writeIndex
  , writeDocuments
  ) where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocDesc               (FieldValue (..))
import qualified Hunt.Common.DocDesc               as DocDesc
import           Hunt.Common.DocId
import qualified Hunt.Common.DocIdMap              as DocIdMap
import qualified Hunt.Common.DocIdSet              as DocIdSet
import           Hunt.Common.Document
import           Hunt.Common.Occurrences           (Occurrences)
import qualified Hunt.Common.Positions             as Positions
import           Hunt.Index.Schema
import           Hunt.IO.Buffer                    (Buffer, Flush)
import qualified Hunt.IO.Buffer                    as Buffer
import           Hunt.IO.Files
import           Hunt.IO.Write
import           Hunt.Scoring.SearchResult
import           Hunt.SegmentIndex.Types.SegmentId

import           Data.ByteString                   (ByteString)
import qualified Data.ByteString                   as ByteString
import           Data.Foldable
import           Data.Key
import           Data.Map                          (Map)
import qualified Data.Map.Strict                   as Map
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Text
import qualified Data.Text.Foreign                 as Text
import qualified Data.Vector                       as V
import qualified Data.Vector.Unboxed.Mutable       as UM
import           Data.Word                         hiding (Word)
import           Foreign.Ptr
import           System.FilePath

import           Prelude                           hiding (Word)

type ContextNum = Int

type Offset = Int

type BytesWritten = Int

-- | Write the inverted index to disk.
writeIndex :: FilePath
           -> SegmentId
           -> Schema
           -> [(Context, [(Word, Occurrences)])]
           -> IO ()
writeIndex ixDir sid schema cxWords = do

  withAppendFile (ixDir </> termVectorFile sid) $ \termsFile ->
    withAppendFile (ixDir </> occurrencesFile sid) $ \occsFile ->
    withAppendFile (ixDir </> positionsFile sid) $ \posFile -> do

    Buffer.withBuffer (16 * 1024) $ \termBuffer ->
      Buffer.withBuffer (8  * 1024) $ \occBuffer ->
      Buffer.withBuffer (4  * 1024) $ \posBuffer -> do

      let
        posFlush :: Flush BytesWritten
        posFlush = append posFile
        {-# NOINLINE posFlush #-}

        occFlush :: Flush BytesWritten
        occFlush = append occsFile
        {-# NOINLINE occFlush #-}

        termFlush :: Flush BytesWritten
        termFlush = append termsFile
        {-# NOINLINE termFlush #-}

        foldPositions :: BytesWritten -> Position -> IO BytesWritten
        foldPositions !bytesWritten position = do
          n <- putWrite
               posBuffer
               posFlush
               vint
               position
          return $! n + bytesWritten

        foldOccurrences :: (Offset, Offset)
                        -> DocId
                        -> Positions.Positions
                        -> IO (Offset, Offset)
        foldOccurrences (!occOffset, !posOffset) docId positions = do
          posBytes <- foldlM
                      foldPositions
                      0
                      (Positions.toAscList positions)

          occBytes <- putWrite
                      occBuffer
                      occFlush
                      occurrenceWrite
                      (docId, (Positions.size positions, posOffset))

          return $! (occOffset + occBytes, posOffset + posBytes)

        foldWords :: (Int, Text, Offset, Offset)
                  -> (Word, Occurrences)
                  -> IO (Int, Text, Offset, Offset)
        foldWords ( !wordsWritten
                  , !lastWord
                  , !occOffset
                  , !posOffset
                  ) (word, occs) = do

          (occOffset', posOffset') <- foldlWithKeyM
                                      foldOccurrences
                                      (occOffset, posOffset)
                                      occs

          -- We encode the terms in a prefix free fashion. We look
          -- for a common prefix with the previous insert word
          -- and only encode the suffix.
          let (prefix, suffix) = case Text.commonPrefixes lastWord word of
                Just (p, _, s) -> (p, s)
                Nothing        -> (Text.empty, word)

          _ <- putWrite
               termBuffer
               termFlush
               termWrite
               ( Text.lengthWord16 prefix * 2
               , ( suffix
                 , ( DocIdMap.size occs
                   , occOffset
                   )
                 )
               )

          return $! (wordsWritten + 1, word, occOffset', posOffset')

        foldContexts :: UM.IOVector Int
                     -> (Offset, Offset)
                     -> Int
                     -> (Context, [(Word, Occurrences)])
                     -> IO (Offset, Offset)
        foldContexts wordCounts (!occOffset, !posOffset) i (cx, words) = do
          (wordsWritten', _, occOffset', posOffset') <-
            foldlM
            foldWords
            ( 0 :: Int , Text.empty , occOffset, posOffset )
            words

          UM.unsafeWrite wordCounts i wordsWritten'

          return $! (occOffset', posOffset')

      cxWordCount <- UM.unsafeNew (Map.size schema)
      (!occOffset, !posOffset) <- foldlWithKeyM
                                  (foldContexts cxWordCount)
                                  (0, 0)
                                  cxWords

      Buffer.flush termBuffer termFlush
      Buffer.flush occBuffer occFlush
      Buffer.flush posBuffer posFlush
      return ()
  return ()

  where
    vint@(W vintSize vintWrite) = fromIntegral >$< varint64

type SortedFields = V.Vector Field

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

  where
    vint = fromIntegral >$< varint64

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

-- | A 'Write' for 'Occurrence'
occurrenceWrite :: Write (DocId, (Int, Offset))
occurrenceWrite = (unDocId >$< vint) >*< vint >*< vint
{-# INLINE occurrenceWrite #-}

termWrite :: Write (Int, (Text, (Int, Offset)))
termWrite = vint >*< text >*< vint >*< vint
{-# INLINE termWrite #-}

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
    size FV_Null       = 0

    write (FV_Int i) op    = word8Write 0 op >>= vintWrite i
    write (FV_Float _f) op = word8Write 1 op >>= undefined
    write (FV_Text s) op   = word8Write 2 op >>= tWrite s
    write (FV_Binary b) op = word8Write 3 op >>= bsWrite b
    write FV_Null op       = return op

    tagSize = word8Size 0
{-# INLINE fieldValueWrite #-}

termVectorFile :: SegmentId -> FilePath
termVectorFile sid = show sid <.> "tv"

occurrencesFile :: SegmentId -> FilePath
occurrencesFile sid = show sid <.> "occ"

positionsFile :: SegmentId -> FilePath
positionsFile sid = show sid <.> "pos"

fieldIndexFile :: SegmentId -> FilePath
fieldIndexFile sid = show sid <.> "fdx"

fieldDataFile :: SegmentId -> FilePath
fieldDataFile sid = show sid <.> "fdt"
