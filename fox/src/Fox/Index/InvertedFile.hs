{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module Fox.Index.InvertedFile (
    IfM
  , runIfM

  , InvFileInfo(..)
  , writeInvertedFiles

  , iterateTerms
  , iterateOccurrences
  , iteratePositions

  , trace

  , TermIndex.TermIndex
  , readTermIndexFile
  ) where

import qualified Fox.IO.Buffer as Buffer
import qualified Fox.IO.Files as Files
import qualified Fox.IO.Read as Read
import qualified Fox.IO.Write as Write
import qualified Fox.Index.Directory as Directory
import qualified Fox.Index.InvertedFile.Records as Records
import qualified Fox.Index.InvertedFile.TermIndex as TermIndex
import qualified Fox.Indexer as Indexer
import qualified Fox.Schema as Schema
import qualified Fox.Types.DocDesc  as Document
import qualified Fox.Types.DocIdMap as DocIdMap
import qualified Fox.Types.Document as Document
import qualified Fox.Types.Occurrences as Occurrences
import qualified Fox.Types.Positions as Positions
import qualified Fox.Types.Token as Token
import qualified Fox.Types.TextSearchOp as TextSearchOp

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Data.Bits as Bits
import qualified Data.Count as Count
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Key as Key
import qualified Data.Offset as Offset
import qualified Data.Text.Foreign as Text
import qualified Data.Vector.Storable as Vector
import qualified Data.Vector.Storable.Mutable as MVector
import qualified Data.Word as Word
import qualified Debug.Trace as Trace
import qualified Foreign.Ptr as Ptr
import qualified GHC.Generics as GHC
import qualified GHC.Stack as CallStack
import qualified Streaming as Stream
import qualified Streaming.Prelude as Stream
import qualified System.MemoryMap as MemoryMap

import Prelude hiding (read)

-- | A monad in which we write the inverted files. Currently this is an
-- alias for 'IO', however in the future this might become something
-- different to support global buffer pools and better cleanup of files
-- in case transaction fail here.
type IfM
  = IO

runIfM :: IfM a -> IO a
runIfM = id

-- | A range of bytes in a buffer represented by the pointer to the first byte
-- of the range and the pointer to the first byte /after/ the range.
data BufferRange =
  BufferRange !(Ptr.Ptr Word.Word8) !(Ptr.Ptr Word.Word8)

-- | Moves the pointer to the first byte of the range to an offset. TODO: Ideally
-- we would check for overflow here. ()
atOffset
  :: BufferRange
  -> Offset.OffsetOf a
  -> BufferRange
atOffset (BufferRange start end) (Offset.OffsetOf off) =
  -- TODO: check buffer range
  BufferRange (start `Ptr.plusPtr` off) end

read
  :: BufferRange
  -> Read.Read a
  -> IfM a
read (BufferRange start end) (Read.R runRead) = do
  (a, _) <- runRead end start
  return a

read'
  :: BufferRange
  -> Read.Read a
  -> IfM (a, BufferRange)
read' (BufferRange start end) (Read.R runRead) = do
  (a, start') <- runRead end start
  return $! (a, BufferRange start' end)

-- | A buffered file abstraction. The `System.IO.Handle` type
-- is synchronized with an MVar. In our special case we don't
-- need any special synchronization thus we build our own
-- buffered file here.
data BufferedAppendFile
  = BufferedAppendFile !Buffer.Buffer !Files.AppendFile

-- | Write to a buffer. First check for overflow and flush
-- as necessary.
write
  :: BufferedAppendFile
  -> Write.Write a
  -> a
  -> IfM ()
write (BufferedAppendFile buffer file) (Write.W size put) a = do
  enough <- Buffer.hasEnoughBytes buffer (size a)
  Monad.unless enough $
    Buffer.flush buffer (Files.append file)
  Buffer.put buffer (put a)

-- | Returns the count of bytes written so far.
offset :: BufferedAppendFile
       -> IfM (Offset.OffsetOf a)
offset (BufferedAppendFile buffer _) =
  Offset.OffsetOf `fmap` Buffer.offset buffer

flush :: BufferedAppendFile -> IfM ()
flush (BufferedAppendFile buffer file) =
  Buffer.flush buffer (Files.append file)

-- | Currently this is just an uninformed guess.
defaultBufferSize :: Int
defaultBufferSize = 32 * 1024

-- | Information about the written inverted file. Available
-- after it has been written to disk.
data InvFileInfo
  = InvFileInfo {
        ifTermCount :: !Records.VocCount
        -- ^ Number of unique terms in this inverted
        -- file

      , ifIxCount   :: !Records.VocCount
        -- ^ Number of unique terms which that do not
        -- share prefixes (e.g. vwlengthPrefix(x) == 0)
      } deriving (GHC.Generic)

instance Semigroup InvFileInfo where
  l <> r =
    InvFileInfo {
        ifTermCount = ifTermCount l <> ifTermCount r
      , ifIxCount   = ifIxCount l <> ifIxCount r
      }

instance Monoid InvFileInfo where
  mempty =
    InvFileInfo {
        ifTermCount = mempty
      , ifIxCount   = mempty
      }

-- | A strict tuple to avoid space leaks in some folds further
-- down the road.
data Pair a b
  = P !a !b

instance ( Semigroup a
         , Semigroup b) => Semigroup (Pair a b) where
  P a b <> P c d =
    P (a <> c) (b <> d)

-- | Get a memory map from a file. TODO: `IfM` should
-- talk to a global, synchronized cache of open file
-- mappings, and release the mappings automatically
fileMapping :: FilePath -> IfM BufferRange
fileMapping fp = do
  mmap <- MemoryMap.fileMapRead fp
  let
    start =
      MemoryMap.fileMappingPtr mmap
    end =
      start `Ptr.plusPtr` fromIntegral (MemoryMap.fileMappingSize mmap)
    bufferRange =
      BufferRange start end

  return bufferRange


-- | Write the indexed vocabulary, occurrences and postings
-- to the index directory.
writeInvertedFiles
  :: Directory.SegmentDirLayout
  -> Schema.FieldOrds
  -> Indexer.TermIndex
  -> IfM InvFileInfo
writeInvertedFiles Directory.SegmentDirLayout{..} fieldOrds vocabulary = do
  withInvertedFileBuffers $ \voc occ pos ix -> do

    let
      -- write a position to the positions file. TODO: make
      -- this a fold to allow gap encoding of consecutive
      -- positions
      writePosition :: Positions.Position -> IfM ()
      writePosition position =
        write pos Records.positionWrite position

      -- write record to the occurrences file
      writeOccurrence
        :: Document.DocId
        -> Positions.Positions
        -> IfM ()
      writeOccurrence docId positions = do
        posOffset <- offset pos

        Foldable.for_
          (Positions.toAscList positions) writePosition

        write occ Records.occurrenceWrite
          Records.OccRec {
              owDocId     = docId
            , owPosCount  = Count.CountOf (Positions.size positions)
            , owPosOffset = posOffset
            }

      -- write a record to the vocabulary file
      writeVocabulary
        :: Count.CountOf Word.Word16
        -> Token.Term
        -> Schema.FieldOrd
        -> Occurrences.Occurrences
        -> IfM ()
      writeVocabulary prefixLength term field occurrences = do
        -- start by writing the occurrences to the occs file
        -- remember the offset where we started for this term.
        occOffset <- offset occ
        Key.forWithKey_ occurrences $ \docId positions ->
          writeOccurrence docId positions

        write voc Records.vocWrite
          Records.VocRec {
              vwLengthPrefix = prefixLength
            , vwTerm         = term
            , vwField        = field
            , vwOccCount     = Count.CountOf (DocIdMap.size occurrences)
            , vwOccOffset    = occOffset
            }

      -- we fold over each term and their occurrences in the vocabulary.
      foldVocabulary
        :: Pair InvFileInfo Token.Term
        -> Token.Term
        -> HashMap.HashMap Document.FieldName Occurrences.Occurrences
        -> IfM (Pair InvFileInfo Token.Term)
      foldVocabulary (P lastInvInfo lastTerm) term fields =
        let
          (prefix, suffix) =
            case Token.commonPrefixes lastTerm term of
              Just ps -> ps
              Nothing -> (Token.empty, term)

          prefixLength :: Count.CountOf Word.Word16
          prefixLength =
            Count.CountOf (Token.lengthWord16 prefix)

          invInfo :: InvFileInfo
          invInfo =
            InvFileInfo {
                ifTermCount = Count.one
              , ifIxCount   = if prefixLength == Count.zero
                              then Count.one
                              else Count.zero
              } <> lastInvInfo

          forField :: Schema.FieldOrd -> Schema.FieldName -> IfM ()
          forField fieldOrd fieldName =
            case HashMap.lookup fieldName fields of
              Just occurrences -> do
                writeVocabulary prefixLength suffix fieldOrd occurrences
              Nothing ->
                return ()
        in do
          -- for vocabulary with a zero length shared prefix
          -- we make an entry in the index file.
          Monad.when (prefixLength == Count.zero) $ do
            vocOffset <- offset voc
            write ix Records.ixWrite
              Records.IxRec {
                  ixVocOffset          = vocOffset
                , ixPrecedingTermCount = ifTermCount lastInvInfo
                }

          Schema.forFields_ fieldOrds forField
          return (P invInfo term)

    P invInfo _ <-
      Key.foldlWithKeyM foldVocabulary (P mempty Token.empty) vocabulary
    return invInfo
  where
    -- Initialize all the resources we need. In the future we would
    -- want a buffer pool here to give an upper bound on memory
    -- consumtion
    withInvertedFileBuffers action =
      Files.withAppendFile segmentVocabularyFile $ \vocfile ->
      Files.withAppendFile segmentOccurrencesFile $ \occfile ->
      Files.withAppendFile segmentPostingsFile $ \posfile ->
      Files.withAppendFile segmentIxFile $ \ixfile ->
      Buffer.withBuffer defaultBufferSize $ \vocbuf ->
      Buffer.withBuffer defaultBufferSize $ \occbuf ->
      Buffer.withBuffer defaultBufferSize $ \posbuf ->
      Buffer.withBuffer defaultBufferSize $ \ixbuf -> do

      let
        voc = BufferedAppendFile vocbuf vocfile
        occ = BufferedAppendFile occbuf occfile
        pos = BufferedAppendFile posbuf posfile
        ix  = BufferedAppendFile ixbuf  ixfile

      x <- action voc occ pos ix

      flush voc
      flush occ
      flush pos
      flush ix
      return x

-- | Loads the TermIndex from disk.
readTermIndexFile
  :: Directory.SegmentDirLayout
  -> InvFileInfo
  -> IfM TermIndex.TermIndex
readTermIndexFile segmentDirLayout invFileInfo = do
  buffer <- fileMapping (Directory.segmentIxFile segmentDirLayout)
  let
    termCount =
      ifTermCount invFileInfo

    ixCount =
      Count.getInt (ifIxCount invFileInfo)

  termIx <- MVector.unsafeNew ixCount
  read buffer $ do
    let
      loop :: Int -> Read.Read ()
      loop i
        | i < ixCount
        = do
            ixRec <- Records.ixRead
            IO.liftIO (MVector.unsafeWrite termIx i ixRec)
            loop $! (i + 1)
        | otherwise
        = return ()
    loop 0

  termIx' <- Vector.unsafeFreeze termIx
  return $! (TermIndex.TermIndex termCount termIx')

liftIfM
  :: Functor f
  => IfM a
  -> Stream.Stream f IfM a
liftIfM =
  Stream.lift

-- | Iterate through the vocabulary and find matching texts
iterateTerms
  :: Directory.SegmentDirLayout
  -> TermIndex.TermIndex
  -> TextSearchOp.TextSearchOp
  -> Token.Term
  -> Stream.Stream (Stream.Of (Token.Term, Records.VocRec Read.UTF16)) IfM ()
iterateTerms segmentDirLayout termIndex searchOp term = do

  bufferRange <-
    liftIfM $ fileMapping (Directory.segmentVocabularyFile segmentDirLayout)

  let
    bisect
      :: BufferRange
      -> TermIndex.Bisect TermIndex.TermIndex
      -> Stream.Stream (Stream.Of (Token.Term, Records.VocRec Read.UTF16)) IfM ()
    bisect buffer termIxNode =
      case TermIndex.label termIxNode of
        Just (vocOffset, scanWidth) -> do
          (voc, buffer') <-
            liftIfM $ read' (atOffset buffer vocOffset) Records.vocRead
          term' <-
            liftIfM $ utf16ToTerm (Records.vwTerm voc)

          let
            -- while bisecting through the vocabulary we
            -- look for matching prefixes only
            searchOp' =
              TextSearchOp.toPrefixSearchOp searchOp

          case TextSearchOp.matches searchOp' term term' of
            TextSearchOp.Smaller ->
              bisect buffer (TermIndex.left termIxNode)
            TextSearchOp.Matches -> do
              Stream.yield (term', voc)
              scan (Count.dec scanWidth) term' buffer'
            TextSearchOp.Larger ->
              bisect buffer (TermIndex.right termIxNode)
        Nothing ->
          return ()

    scan
      :: Count.CountOf (Records.VocRec Read.UTF16)
      -> Token.Term
      -> BufferRange
      -> Stream.Stream (Stream.Of (Token.Term, Records.VocRec Read.UTF16)) IfM ()
    scan width prefix buffer
      | Count.isZero width = return ()
      | otherwise = do
          (voc, buffer') <- liftIfM $ read' buffer Records.vocRead
          term' <- liftIfM $ utf16ToTerm (Records.vwTerm voc)
          Stream.yield (term', voc)
          scan (Count.dec width) prefix buffer'

  bisect bufferRange (TermIndex.bisect termIndex)

iterateOccurrences
  :: Directory.SegmentDirLayout
  -> Records.VocRec a
  -> Stream.Stream (Stream.Of Records.OccRec) IfM ()
iterateOccurrences segmentDirLayout Records.VocRec{..} = do

  bufferRange <-
    liftIfM $ fileMapping (Directory.segmentOccurrencesFile segmentDirLayout)

  let
    loop i buffer
      | i < vwOccCount = do
          (occ, buffer') <- liftIfM $ read' buffer Records.occurrenceRead
          Stream.yield occ
          loop (Count.inc i) buffer'
      | otherwise =
          return ()

  loop Count.zero bufferRange

iteratePositions
  :: Directory.SegmentDirLayout
  -> Records.OccRec
  -> Stream.Stream (Stream.Of Positions.Position) IfM ()
iteratePositions segmentDirLayout Records.OccRec{..} = do

  bufferRange <-
    liftIfM $ fileMapping (Directory.segmentPostingsFile segmentDirLayout)

  let
    loop i buffer
      | i < owPosCount = do
          (pos, buffer') <- liftIfM $ read' buffer Records.positionRead
          Stream.yield pos
          loop (Count.inc i) buffer'
      | otherwise =
          return ()

  loop Count.zero bufferRange


trace :: (CallStack.HasCallStack, Show a)
      => a
      -> IfM ()
trace x =
  CallStack.withFrozenCallStack (Trace.traceM msg)
  where
    msg =
      concat
        [ show x
        , "\n"
        , CallStack.prettyCallStack CallStack.callStack
        ]

utf16ToTerm :: Read.UTF16 -> IfM Token.Term
utf16ToTerm (Read.UTF16 op len) =
  Text.fromPtr
    (Ptr.castPtr op)
    (fromIntegral (len `Bits.unsafeShiftR` 1))
