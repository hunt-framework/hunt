{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RecordWildCards #-}
module Hunt.SegmentIndex.Store.TermInfos where

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

type Offset = Int

type BytesWritten = Int

-- | Write the inverted index to disk.
writeIndex :: FilePath
           -> SegmentId
           -> Schema
           -> [(Context, [(Word, Occurrences)])]
           -> IO [(Context, [(Word, TermInfo)])]
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
                  -> IO ((Word, TermInfo), (Int, Text, Offset, Offset))
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
              !numOccs         = DocIdMap.size occs

          _ <- putWrite
               termBuffer
               termFlush
               termWrite
               ( Text.lengthWord16 prefix * 2
               , ( suffix
                 , ( numOccs
                   , occOffset
                   )
                 )
               )

          return $! ( (word, TermInfo occOffset numOccs)
                    , (wordsWritten + 1, word, occOffset', posOffset')
                    )

        foldContexts :: UM.IOVector Int
                     -> (Offset, Offset)
                     -> Int
                     -> (Context, [(Word, Occurrences)])
                     -> IO ((Context, [(Word, TermInfo)]), (Offset, Offset))
        foldContexts wordCounts (!occOffset, !posOffset) i (cx, words) = do

          (words', (wordsWritten', _, occOffset', posOffset')) <-
            mapAccumM
            foldWords
            ( 0 :: Int , Text.empty , occOffset, posOffset )
            words

          UM.unsafeWrite wordCounts i wordsWritten'

          return $! ( (cx, words')
                    , (occOffset, posOffset')
                    )

      cxWordCount <- UM.unsafeNew (Map.size schema)

      (!cxWords', (!occOffset, !posOffset)) <-
        mapAccumWithKeyM (foldContexts cxWordCount) (0,0) cxWords

      Buffer.flush termBuffer termFlush
      Buffer.flush occBuffer occFlush
      Buffer.flush posBuffer posFlush

      return cxWords'

  where
    vint@(W vintSize vintWrite) = fromIntegral >$< varint64

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

mapAccumWithKeyM :: (Monad m, TraversableWithKey t)
                 => (a -> Key t -> b -> m (c, a))
                 -> a
                 -> t b
                 -> m (t c, a)
mapAccumWithKeyM f z xs =
  runStateT (traverseWithKey (\k x -> StateT (\s -> f s k x)) xs) z
{-# INLINE mapAccumWithKeyM #-}

mapAccumM :: (Monad m, Traversable t)
          => (a -> b -> m (c, a))
          -> a
          -> t b
          -> m (t c, a)
mapAccumM f z xs = runStateT (traverse (\x -> StateT (\s -> f s x)) xs) z
{-# INLINE mapAccumM #-}
