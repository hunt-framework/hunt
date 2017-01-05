{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes   #-}
module Fox.Index.Directory (
    IndexDirectory
  , IDir
  , IDirError(..)
  , runIDir

  , openIndexDirectory
  , writeTermIndex
  , writeDocuments
  ) where

import           Fox.Analyze            (Token)
import           Fox.IO.Buffer          (WriteBuffer, flush, offset,
                                         withWriteBuffer, write)
import           Fox.IO.Files (AppendFile)
import qualified Fox.IO.Files as Files
import           Fox.IO.Write
import           Fox.Types
import qualified Fox.Types.DocIdMap     as DocIdMap
import           Fox.Types.Document
import qualified Fox.Types.Positions    as Positions

import           Control.Exception      (onException)
import           Control.Monad.Except
import           Data.Bits
import           Data.Foldable
import           Data.Key
import           Data.Map               (Map)
import           Data.Sequence          (Seq)
import           Data.Text              (Text)
import qualified Data.Text              as Text
import qualified Data.Text.Foreign      as Text
import           System.Directory
import           System.FilePath
import           System.IO.Error        (IOError, tryIOError)

-- | Root directory where the index files are stored.
newtype IndexDirectory = IndexDirectory { _unIndexDirectory :: FilePath }
                       deriving (Eq, Ord, Show)

-- | Computation involving the physical directory where the
-- index is stored are carried out in the @IDir@ monad.
newtype IDir a = IDir { unIDir :: forall r. (a -> IO r) -> IndexDirectory -> IO r }

instance Functor IDir where
  fmap f (IDir m) = IDir (\k i -> m (\a -> k (f a)) i)

instance Applicative IDir where
  pure a = IDir (\k _ -> k a)
  IDir fm <*> IDir fa = IDir $ \k i -> fm (\f -> fa (\a -> k (f a) ) i) i

instance Monad IDir where
  IDir m >>= f = IDir $ \k i -> m (\a -> unIDir (f a) k i) i

instance MonadIO IDir where
  liftIO m = IDir $ \k _ -> m >>= k

runIDir :: IndexDirectory -> IDir a -> IO (Either IDirError a)
runIDir indexDirectory action = do
  ea <- tryIOError $ (unIDir action) pure indexDirectory
  case ea of
    Left e   -> return (Left (IDirIOError e))
    Right a  -> return (Right a)

-- | Errors occurring while opening an @IndexDirectory@.
data IDirError = IDirInvalidDirectory
               | IDirIndexLocked
               | IDirIOError IOError
               deriving (Show)

-- | Opens a directory for reading and writing.
openIndexDirectory :: FilePath -> IO (Either IDirError IndexDirectory)
openIndexDirectory indexDir = runExceptT $ do
    -- bail out early so we don't overwrite anything
  -- in case indexDir refers to a file.
  fileExists <- liftIO $ doesFileExist indexDir
  when fileExists $
    throwError IDirInvalidDirectory

  -- check explicitly for existence to avoid
  -- exceptions.
  dirExists <- liftIO $ doesDirectoryExist indexDir
  unless dirExists $
    liftIO $ createDirectoryIfMissing True indexDir

  return $ IndexDirectory indexDir

-- | Position in a file numbered from zero.
type Offset = Int

docIdWrite :: Write DocId
docIdWrite = unDocId >$< varint
{-# INLINE docIdWrite #-}

posWrite :: Write Position
posWrite = varint
{-# INLINE posWrite #-}

occWrite :: Write (DocId, (Int, Offset))
occWrite = docIdWrite >*< varint >*< varint
{-# INLINE occWrite #-}

-- unused due to GHC not being able to optimize big tuple
-- away.
_termWrite :: Write (Int, (Text, (FieldOrd, (Int, Offset))))
_termWrite = varint >*< text >*< varint >*< varint >*< varint
{-# INLINE _termWrite #-}

-- | Write a @FieldIndex@ to disk.
writeTermIndex :: SegmentId
               -> (FieldName -> FieldOrd)
               -> Map Token (Map FieldName Occurrences)
               -> IDir ()
writeTermIndex segmentId fieldOrd fieldIndex = do

  -- open all files needed for index writing
  withAppendFile (termVectorFile segmentId) $ \tvFile ->
    withAppendFile (occurrenceFile segmentId) $ \occFile ->
    withAppendFile (positionFile segmentId) $ \posFile -> liftIO $ do
      let
        defaultBufSize :: Int
        defaultBufSize = 32 * 1024

        -- Full buffers are flushed with these
        tvFlush  = Files.append tvFile
        occFlush = Files.append occFile
        posFlush = Files.append posFile

      -- allocate WriteBuffers for any file we want to write.
      -- WriteBuffers keep track of the current offset which
      -- is handy for cross referencing records between files.
      withWriteBuffer defaultBufSize tvFlush $ \tvBuf ->
        withWriteBuffer defaultBufSize occFlush $ \occBuf ->
        withWriteBuffer defaultBufSize posFlush $ \posBuf -> do

        let
          writePosition :: Position -> IO ()
          writePosition position = do
            write_ posBuf posWrite position

          writeOccs :: DocId -> Positions -> IO ()
          writeOccs docId positions = do
            positionsOffset <- offset posBuf
            for_ (Positions.toAscList positions) writePosition
            write_ occBuf occWrite
              (docId, (Positions.size positions, positionsOffset))

          -- write the delta of a term to buffer. sameToken parameter
          -- is an optimization when we know token is the same as lastToken.
          writeTerm :: Bool -> Token -> Token -> FieldName -> Occurrences -> IO ()
          writeTerm sameToken lastToken token fieldName occurrences = do

            -- start by writing the occurrences to the occs file
            -- remember the offset where we started for this term.
            occOffset <- offset occBuf
            forWithKey_ occurrences $ \docId positions -> writeOccs docId positions

            let
              -- by calculating the common prefix with the last token
              -- we only need to write the suffix of the current token
              -- effectively delta encoding the terms.

              -- if a token occurrs in different fields we know we can
              -- skip the common prefix calculation.
              (prefix, suffix) | sameToken = (lastToken, Text.empty)
                               | otherwise =
                                   case Text.commonPrefixes lastToken token of
                                     Just (p, _, s) -> (p, s)
                                     Nothing        -> (Text.empty, token)

              prefixLen = Text.lengthWord16 prefix `unsafeShiftL` 2 -- TODO: abstract away
              numOccs   = DocIdMap.size occurrences
              fieldOrd_ = fieldOrd fieldName

            -- we need to split these guys up because GHC is not smart enough
            -- to optimize the deeply nested tuples away.
            write_ tvBuf (varint >*< text)
              (prefixLen, suffix)
            write_ tvBuf (varint >*< varint >*< varint)
              (fieldOrd_, (numOccs, occOffset))


        -- loop over all tokens and fields
        let
          foldTokens :: Text -> Text -> Map FieldName Occurrences -> IO Text
          foldTokens lastToken token fields =
            let
              foldFields notFirst fieldName occs = do
                writeTerm notFirst lastToken token fieldName occs
                return True
            in do
              _ <- foldlWithKeyM foldFields False fields
              return token
        _ <- foldlWithKeyM foldTokens Text.empty fieldIndex

        flush tvBuf
        flush occBuf
        flush posBuf

        return ()
  where
    write_ :: WriteBuffer -> Write a -> a -> IO ()
    write_ writeBuffer (W size put) a = do
      _ <- write writeBuffer (size a) (put a)
      return ()

-- | Write buffered docs to disk.
writeDocuments :: SegmentId
               -> (FieldName -> FieldOrd)
               -> Seq Document
               -> IDir ()
writeDocuments _segmentId _fieldOrd _documents = do
  return ()

-- | Atomically creates an @AppendFile@ in the @IndexDirectory@.
-- Files are created atomically, if an exception occurrs the
-- created file is destroyed.
withAppendFile :: IDir FilePath -> (AppendFile -> IDir a) -> IDir a
withAppendFile mkPath action = IDir $ \k indexDirectory ->
  let
    withPath path =
      let
        tmpPath  = path <.> "tmp"
        finish a = renameFile tmpPath path >> k a
        handler  = removeFile tmpPath
      in
        Files.withAppendFile tmpPath $ \af -> do
          unIDir (action af) finish indexDirectory `onException` handler
  in unIDir mkPath withPath indexDirectory

-- selectors for file names in the @IndexDirectory@.

withIndexRoot :: (FilePath -> FilePath) -> IDir FilePath
withIndexRoot f = IDir $ \k (IndexDirectory indexDirectory) ->
  k (f indexDirectory)

termVectorFile :: SegmentId -> IDir FilePath
termVectorFile segmentId =
  withIndexRoot $ \root -> root </> show segmentId <.> "tv"

occurrenceFile :: SegmentId -> IDir FilePath
occurrenceFile segmentId =
  withIndexRoot $ \root -> root </> show segmentId <.> "occs"

positionFile :: SegmentId -> IDir FilePath
positionFile segmentId =
  withIndexRoot $ \root -> root </> show segmentId <.> "pos"
