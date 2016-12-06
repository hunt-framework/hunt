{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE PatternGuards   #-}
{-# LANGUAGE RecordWildCards #-}
module Hunt.SegmentIndex.Directory.TermInfos where

import           Control.Exception
import           Control.Monad.Except
import           Control.Monad.State.Strict
import qualified Data.Binary                        as Binary
import qualified Data.Binary.Get                    as Binary
import qualified Data.Binary.Get.Internal           as Binary
import qualified Data.Binary.Put                    as Binary
import           Data.Bits
import           Data.Bytes.Serial
import           Data.Bytes.VarInt
import           Data.ByteString                    (ByteString)
import qualified Data.ByteString                    as ByteString
import qualified Data.ByteString.Builder            as Builder
import qualified Data.ByteString.Lazy               as LByteString
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
import           Hunt.SegmentIndex.Descriptor
import           Hunt.SegmentIndex.Directory.Layout
import           Hunt.SegmentIndex.Types
import           Hunt.SegmentIndex.Types.Generation
import           Hunt.SegmentIndex.Types.Index
import           Hunt.SegmentIndex.Types.SegmentId
import           Hunt.SegmentIndex.Types.SegmentMap (SegmentMap)
import qualified Hunt.SegmentIndex.Types.SegmentMap as SegmentMap
import           Hunt.SegmentIndex.Types.TermInfo
import           System.FilePath
import           System.IO                          (IOMode (WriteMode),
                                                     withFile)

import           Prelude                            hiding (Word, words)

type Offset = Int

type BytesWritten = Int

newtype TermInfoDecodingError = TermInfoDecodingError String
                              deriving (Eq, Show)

instance Exception TermInfoDecodingError

-- | A stream of @Text@ and @TermInfo@.
data TermInfos = TI_Cons !Word !TermInfo TermInfos
               | TI_Nil (Maybe TermInfoDecodingError) !LByteString.ByteString

-- | Lazily streams n @TermInfo@s from a @Data.ByteString.Lazy@.
readTermInfos :: Int -> LByteString.ByteString -> TermInfos
readTermInfos nterms bytes =
  let
    -- | The standard @Text@ @Binary@ instance first converts
    -- to UTF-8 charset.
    getRawText :: Binary.Get Text
    getRawText = do
      VarInt nbytes <- deserialize
      Binary.readNWith nbytes $ \op ->
        Text.fromPtr (castPtr op) (fromIntegral (nbytes `unsafeShiftR` 1))

    -- we really want to make sure we are lazy in the spine
    -- and strict in the leafes here to avoid space leaks
    getTermInfo :: Word -> Binary.Get (Word, TermInfo)
    getTermInfo lastWord = do
      VarInt prefixLen <- deserialize
      suffix           <- getRawText
      VarInt occCount  <- deserialize
      VarInt occOffset <- deserialize
      case Text.take prefixLen lastWord `Text.append` suffix of
        word -> case TermInfo occCount occOffset of
          termInfo -> case (word, termInfo) of
            wordAndTermInfo -> pure $! wordAndTermInfo

    -- lazily decodes the bytes into terms and @TermInfo@.
    loop :: Int -> Word -> LByteString.ByteString -> TermInfos
    loop !n !lastWord !bs
      | n <= 0    = TI_Nil Nothing bs
      | otherwise = case Binary.runGetOrFail (getTermInfo lastWord) bs of
                      Right (bs', _, (word, termInfo)) ->
                        TI_Cons word termInfo (loop (n - 1) word bs')
                      Left (bs', _, err) ->
                        TI_Nil (Just (TermInfoDecodingError err)) bs'

  in loop nterms Text.empty bytes

-- | Reads the term vector file for a specific @SegmentId@.
-- Needs the term counts for each context. Doesn't read contexts
-- which are not in the @Schema@.
readTermVector :: FilePath
               -> Schema
               -> SegmentId
               -> Map Context Int
               -> IO (Either TermInfoDecodingError ContextMap)
readTermVector indexDirectory schema segmentId termVectorSizes =
  let
    -- loop over every term and @TermInfo@ and incrementally
    -- construct an efficient index structure for lookups.
    buildContextMap :: LByteString.ByteString
                    -> Context
                    -> ContextSchema
                    -> ExceptT TermInfoDecodingError IO (IndexRepr, LByteString.ByteString)
    buildContextMap bytes context contextSchema
      | Just termCount <- Map.lookup context termVectorSizes
      , termCount > 0 = do
          -- TODO: support other index types
          case textIndexDescr of
            IndexDescriptor toIxTerm builder -> case builder of
              (Builder startBuilder insertTerm finishBuilder) ->
                let
                  -- traverse the stream and build the index
                  -- incrementally
                  loop (TI_Nil merr bytes') !indexBuilder
                    | Just decodingError <- merr =
                        return $! Left $! decodingError
                    | otherwise = do
                        index <- finishBuilder indexBuilder
                        return $! Right $!
                          case IndexRepr toIxTerm index of
                            indexRepr -> (indexRepr, bytes')
                  loop (TI_Cons term termInfo rest) !indexBuilder = do
                    indexBuilder' <- insertTerm indexBuilder (toIxTerm term, termInfo)
                    loop rest indexBuilder'
                in do
                  mindex <- liftIO
                            $ loop (readTermInfos termCount bytes) =<< startBuilder
                  case mindex of
                    Right (index, bytes') -> return (index, bytes')
                    Left err              -> throwError err
      | otherwise =
          return $! (IndexRepr (\_ -> ()) emptyIndex, bytes)

  in do
    content     <- LByteString.readFile
                   (indexDirectory </> termVectorFile segmentId)
    mContextMap <- runExceptT
                   $ mapAccumWithKeyM buildContextMap content schema
    case mContextMap of
      Right (contextMap, _) -> return $! Right $! contextMap
      Left err              -> return $! Left err

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
