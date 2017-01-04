{-# LANGUAGE BangPatterns #-}
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
newtype IDir a = IDir { unIDir :: IndexDirectory -> IO a }

instance Functor IDir where
  fmap f (IDir m) = IDir (\i -> fmap f (m i))

instance Applicative IDir where
  pure a = IDir (\_ -> pure a)
  IDir fm <*> IDir fa = IDir $ \i -> fm i <*> fa i

instance Monad IDir where
  IDir m >>= f = IDir $ \i -> m i >>= \a -> unIDir (f a) i

instance MonadIO IDir where
  liftIO m = IDir $ \_ -> m

runIDir :: IndexDirectory -> IDir a -> IO (Either IDirError a)
runIDir indexDirectory action = do
  ea <- tryIOError $ (unIDir action) indexDirectory
  case ea of
    Right a -> return (Right a)
    Left e  -> return (Left (IDirIOError e))
  
-- | Errors occurring while opening an @IndexDirectory@.
data IDirError = IDirInvalidDirectory
               | IDirIndexLocked
               | IDirIOError IOError

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

              prefixLen = Text.lengthWord16 prefix `unsafeShiftL` 2
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

-- | Create an @AppendFile@ in the @IndexDirectory@ for appending.
withAppendFile :: IDir FilePath -> (AppendFile -> IDir a) -> IDir a
withAppendFile mkPath action = IDir $ \indexDirectory -> do
  path <- unIDir mkPath indexDirectory
  Files.withAppendFile path $ \af -> unIDir (action af) indexDirectory

-- selectors for file names in the @IndexDirectory@.

termVectorFile :: SegmentId -> IDir FilePath
termVectorFile segmentId = IDir $ \indexDirectory ->
  return $ indexDirectory <//> show segmentId <.> "tv"

occurrenceFile :: SegmentId -> IDir FilePath
occurrenceFile segmentId = IDir $ \indexDirectory ->
  return $ indexDirectory <//> show segmentId <.> "occs"

positionFile :: SegmentId -> IDir FilePath
positionFile segmentId = IDir $ \indexDirectory ->
  return $ indexDirectory <//> show segmentId <.> "pos"

(<//>) :: IndexDirectory -> FilePath -> FilePath
(<//>) (IndexDirectory indexDirectory) path = indexDirectory </> path
