{-# LANGUAGE RecordWildCards #-}
module Fox.Index.InvertedFile (
    IfM
  , runIfM
  , writeInvertedFiles
  ) where

import qualified Fox.IO.Buffer as Buffer
import qualified Fox.IO.Files as Files
import qualified Fox.IO.Write as Write
import qualified Fox.Index.Directory as Directory
import qualified Fox.Indexer as Indexer
import qualified Fox.Schema as Schema
import qualified Fox.Types.DocDesc  as Document
import qualified Fox.Types.DocIdMap as DocIdMap
import qualified Fox.Types.Document as Document
import qualified Fox.Types.Occurrences as Occurrences
import qualified Fox.Types.Positions as Positions
import qualified Fox.Types.Token as Token

import qualified Data.Coerce as Coerce
import qualified Data.Foldable as Foldable
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Key as Key
import qualified Data.Word as Word

-- | A monad in which we write the inverted files. Currently this is an
-- alias for 'IO', however in the future this might become something
-- different to support global buffer pools and better cleanup of files
-- in case transaction fail here.
type IfM a = IO a

runIfM :: IfM a -> IO a
runIfM = id

-- | A buffered file abstraction. The `System.IO.Handle` type
-- is synchronized with an MVar. In our special case we don't
-- need any special synchronization thus we build our own
-- buffered file here.
data BufferedAppendFile
  = BufferedAppendFile !Buffer.Buffer !Files.AppendFile

-- | Offset with a type annotation, useful for not mixing up
-- offsets into different files.
newtype OffsetOf a
  = OffsetOf Int

-- | Same goes for 'CountOf'. Better not mix any of these up.
newtype CountOf a
  = CountOf Int

-- | Write to a buffer. First check for overflow and flush
-- as necessary.
write :: BufferedAppendFile
      -> Write.Write a
      -> a
      -> IfM ()
write (BufferedAppendFile buffer file) (Write.W size put) a = do
  enough <- Buffer.hasEnoughBytes buffer (size a)
  if not enough
    then Buffer.flush buffer (Files.append file)
    else return ()
  Buffer.put buffer (put a)

-- | Returns the count of bytes written so far.
offset :: BufferedAppendFile
       -> IfM (OffsetOf a)
offset (BufferedAppendFile buffer _) =
  OffsetOf `fmap` Buffer.offset buffer

flush :: BufferedAppendFile -> IfM ()
flush (BufferedAppendFile buffer file) =
  Buffer.flush buffer (Files.append file)

-- | Currently this is just an uninformed guess.
defaultBufferSize :: Int
defaultBufferSize = 32 * 1024

-- | Various 'Write's for the types we want to serialize
-- down the road.
offsetOfWrite :: Write.Write (OffsetOf a)
offsetOfWrite =
  Coerce.coerce Write.>$< Write.varint

countOfWrite :: Write.Write (CountOf a)
countOfWrite =
  Coerce.coerce Write.>$< Write.varint

docIdWrite :: Write.Write Document.DocId
docIdWrite =
  Document.unDocId Write.>$< Write.varint

positionWrite :: Write.Write Positions.Position
positionWrite = Write.varint

termWrite :: Write.Write Token.Term
termWrite = Write.text

-- | Convenience type for serialiasing occurrences for
-- the occurrence file.
data OccWrite
  = OccWrite { owDocId     :: !Document.DocId
             , owPosCount  :: !(CountOf Positions.Position)
             , owPosOffset :: !(OffsetOf Positions.Position)
             }

occurrenceWrite :: Write.Write OccWrite
occurrenceWrite =
  (owDocId     Write.>$< docIdWrite) <>
  (owPosCount  Write.>$< countOfWrite) <>
  (owPosOffset Write.>$< offsetOfWrite)

-- | 'VocWrite' represents a row in the vocabulary file
data VocWrite
  = VocWrite { vwLengthPrefix :: !(CountOf Word.Word16)
             , vwTerm         :: !Token.Term
             , vwField        :: !Schema.FieldOrd
             , vwOccCount     :: !(CountOf Occurrences.Occurrences)
             , vwOccOffset    :: !(OffsetOf Occurrences.Occurrences)
             }

vocWrite :: Write.Write VocWrite
vocWrite =
  (vwLengthPrefix Write.>$< countOfWrite) <>
  (vwTerm         Write.>$< termWrite) <>
  (vwOccCount     Write.>$< countOfWrite) <>
  (vwOccOffset    Write.>$< offsetOfWrite)

-- | Write the indexed vocabulary, occurrences and postings
-- to the index directory.
writeInvertedFiles
  :: Directory.SegmentDirLayout
  -> Schema.FieldOrds
  -> Indexer.TermIndex
  -> IfM ()
writeInvertedFiles Directory.SegmentDirLayout{..} fieldOrds vocabulary = do
  withInvertedFileBuffers $ \voc occ pos -> do

    let
      -- write a position to the positions file. TODO: make
      -- this a fold to allow gap encoding of consecutive
      -- positions
      writePosition :: Positions.Position -> IfM ()
      writePosition position =
        write pos positionWrite position

      -- write record to the occurrences file
      writeOccurrence :: Document.DocId -> Positions.Positions -> IfM ()
      writeOccurrence docId positions = do
        posOffset <- offset pos

        Foldable.for_
          (Positions.toAscList positions) writePosition

        write occ occurrenceWrite
          OccWrite { owDocId     = docId
                   , owPosCount  = CountOf (Positions.size positions)
                   , owPosOffset = posOffset
                   }

      -- write a record to the vocabulary file
      writeVocabulary
        :: CountOf Word.Word16
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

        write voc vocWrite
          VocWrite { vwLengthPrefix = prefixLength
                   , vwTerm         = term
                   , vwField        = field
                   , vwOccCount     = CountOf (DocIdMap.size occurrences)
                   , vwOccOffset    = occOffset
                   }

      foldVocabulary
        :: Token.Term
        -> Token.Term
        -> HashMap.HashMap Document.FieldName Occurrences.Occurrences
        -> IfM Token.Term
      foldVocabulary lastTerm term fields =
        let
          (prefix, suffix) =
            case Token.commonPrefixes lastTerm term of
              Just ps -> ps
              Nothing -> (Token.empty, term)

          prefixLength :: CountOf Word.Word16
          prefixLength =
            CountOf (Token.lengthWord16 prefix)

          forField :: Schema.FieldOrd -> Schema.FieldName -> IfM ()
          forField fieldOrd fieldName =
            case HashMap.lookup fieldName fields of
              Just occurrences ->
                writeVocabulary prefixLength suffix fieldOrd occurrences
              Nothing   ->
                return ()
        in do
          Schema.forFields_ fieldOrds forField
          return term

    _ <- Key.foldlWithKeyM foldVocabulary Token.empty vocabulary
    return ()
  where
    -- Initialize all the resources we need. In the future we would
    -- want a buffer pool here to give an upper bound on memory
    -- consumtion
    withInvertedFileBuffers action =
      Files.withAppendFile segmentVocabularyFile $ \vocfile ->
      Files.withAppendFile segmentOccurrencesFile $ \occfile ->
      Files.withAppendFile segmentPostingsFile $ \posfile ->
      Buffer.withBuffer defaultBufferSize $ \vocbuf ->
      Buffer.withBuffer defaultBufferSize $ \occbuf ->
      Buffer.withBuffer defaultBufferSize $ \posbuf -> do


      let
        voc = BufferedAppendFile vocbuf vocfile
        occ = BufferedAppendFile occbuf occfile
        pos = BufferedAppendFile posbuf posfile

      _ <- action voc occ pos

      flush voc
      flush occ
      flush pos
