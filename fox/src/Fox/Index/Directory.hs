{-# LANGUAGE RankNTypes #-}
module Fox.Index.Directory (
    IndexDirectory
  , IDir
  , IDirError(..)
  , runIDir

  , openIndexDirectory
  , writeTermIndex
  , writeDocuments
  ) where

import qualified Fox.IO.Buffer.WriteBuffer as Writer
import           Fox.IO.Files              (AppendFile)
import qualified Fox.IO.Files              as Files
import           Fox.IO.Write
import           Fox.Schema                (FieldOrd, FieldOrds, foldFields',
                                            forFields_)
import           Fox.Types
import qualified Fox.Types.DocIdMap        as DocIdMap
import           Fox.Types.Document
import qualified Fox.Types.Positions       as Positions
import           Fox.Types.SegmentId       (segmentIdToBase36)
import qualified Fox.Types.Term            as Term

import           Control.Exception         (onException)
import           Control.Monad.Except
import           Data.Foldable
import           Data.HashMap.Strict       (HashMap)
import qualified Data.HashMap.Strict       as HashMap
import           Data.Key
import           Data.Map                  (Map)
import           Data.Sequence             (Seq)
import           System.Directory
import           System.FilePath
import           System.IO.Error           (IOError, tryIOError)


-- | Root directory where the index files are stored.
newtype IndexDirectory = IndexDirectory { _unIndexDirectory :: FilePath }
                       deriving (Eq, Ord, Show)

-- | Computation involving the physical directory where the
-- index is stored are carried out in the @IDir@ monad.
newtype IDir a = IDir { unIDir :: forall r. (a -> IO r)
                               -> IndexDirectory
                               -> IO r
                      }

instance Functor IDir where
  fmap f (IDir m) = IDir $ \k i ->
    m (\a -> k (f a)) i
  {-# INLINABLE fmap #-}

instance Applicative IDir where
  pure a = IDir $ \k _ -> k a
  {-# INLINABLE pure #-}

  IDir fm <*> IDir fa = IDir $ \k i ->
    fm (\f -> fa (\a -> k (f a) ) i) i
  {-# INLINABLE (<*>) #-}

instance Monad IDir where
  IDir m >>= f = IDir $ \k i ->
    m (\a -> unIDir (f a) k i) i
  {-# INLINABLE (>>=) #-}

instance MonadIO IDir where
  liftIO m = IDir $ \k _ ->
    m >>= k
  {-# INLINABLe liftIO #-}

runIDir :: IndexDirectory -> IDir a -> IO (Either IDirError a)
runIDir indexDirectory action = do
  ea <- tryIOError $ (unIDir action) pure indexDirectory
  case ea of
    Left e  -> return (Left (IDirIOError e))
    Right a -> return (Right a)

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
_termWrite :: Write (Int, (Term, (FieldOrd, (Int, Offset))))
_termWrite = varint >*< termWrite >*< varint >*< varint >*< varint
{-# INLINE _termWrite #-}

termWrite :: Write Term
termWrite = Term.termWrite
{-# INLINE termWrite #-}

fieldValueWrite :: Write FieldValue
fieldValueWrite = W size put
  where
    W word8Size word8Put = word8
    W vintSize vintPut   = fromIntegral >$< varint
    W bsSize bsPut       = bytestring
    W textSize textPut   = text

    tagSize = word8Size 0

    size (FV_Int i)    = tagSize + vintSize i
    size (FV_Float _f) = tagSize -- TODO
    size (FV_Text t)   = tagSize + textSize t
    size (FV_Json b)   = tagSize + bsSize b
    size (FV_Binary b) = tagSize + bsSize b
    size FV_Null       = 0

    put (FV_Int i) op    = word8Put 0 op >>= vintPut i
    put (FV_Float _f) op = word8Put 1 op -- TODO
    put (FV_Text s) op   = word8Put 2 op >>= textPut s
    put (FV_Json b) op   = word8Put 3 op >>= bsPut b
    put (FV_Binary b) op = word8Put 4 op >>= bsPut b
    put FV_Null op       = return op

-- | Write a @FieldIndex@ to disk.
writeTermIndex :: SegmentId
               -> FieldOrds
               -> Map Term (HashMap FieldName Occurrences)
               -> IDir ()
writeTermIndex segmentId fieldOrds fieldIndex = do

  -- open all files needed for index writing
  withAppendFile (termVectorFile segmentId) $ \tvFile ->
    withAppendFile (occurrenceFile segmentId) $ \occFile ->
    withAppendFile (positionFile segmentId) $ \posFile -> liftIO $ do
      let
        -- Full buffers are flushed with these
        tvFlush  = Files.append tvFile
        occFlush = Files.append occFile
        posFlush = Files.append posFile

      -- allocate WriteBuffers for any file we want to write.
      -- WriteBuffers keep track of the current offset which
      -- is handy for cross referencing records between files.
      Writer.withWriteBuffer defaultBufSize tvFlush $ \tvBuf ->
        Writer.withWriteBuffer defaultBufSize occFlush $ \occBuf ->
        Writer.withWriteBuffer defaultBufSize posFlush $ \posBuf -> do

        let
          writePosition :: Position -> IO ()
          writePosition position = do
            write_ posBuf posWrite position

          writeOccs :: DocId -> Positions -> IO ()
          writeOccs docId positions = do
            positionsOffset <- Writer.offset posBuf
            for_ (Positions.toAscList positions) writePosition
            write_ occBuf occWrite
              (docId, (Positions.size positions, positionsOffset))

          -- write the delta of a term to buffer. sameToken parameter
          -- is an optimization when we know token is the same as lastToken.
          writeTerm :: Bool -> Term -> Term -> FieldOrd -> Occurrences -> IO ()
          writeTerm sameToken lastToken token fieldOrd occurrences = do

            -- start by writing the occurrences to the occs file
            -- remember the offset where we started for this term.
            occOffset <- Writer.offset occBuf
            forWithKey_ occurrences $ \docId positions -> writeOccs docId positions

            let
              -- by calculating the common prefix with the last token
              -- we only need to write the suffix of the current token
              -- effectively delta encoding the terms.

              -- if a token occurrs in different fields we know we can
              -- skip the common prefix calculation.
              (prefix, suffix) | sameToken = (lastToken, Term.empty)
                               | otherwise =
                                   case Term.commonPrefixes lastToken token of
                                     Just (p, s) -> (p, s)
                                     Nothing     -> (Term.empty, token)

              prefixLen = Term.length prefix
              numOccs   = DocIdMap.size occurrences

            -- we need to split these guys up because GHC is not smart enough
            -- to optimize the deeply nested tuples away.
            write_ tvBuf (varint >*< termWrite)
              (prefixLen, suffix)
            write_ tvBuf (varint >*< varint >*< varint)
              (fieldOrd, (numOccs, occOffset))

        -- loop over all tokens and fields
        let
          foldTokens :: Term -> Term -> HashMap FieldName Occurrences -> IO Term
          foldTokens lastToken token fields =
            let
              foldFields notFirst fieldOrd fieldName
                | Just occs <- HashMap.lookup fieldName fields = do
                  writeTerm notFirst lastToken token fieldOrd occs
                  return True
                | otherwise = return notFirst
            in do
              _ <- foldFields' foldFields False fieldOrds
              return token
        _ <- foldlWithKeyM foldTokens Term.empty fieldIndex

        return ()
  where
    write_ :: Writer.WriteBuffer -> Write a -> a -> IO ()
    write_ writeBuffer (W size put) a =
      Writer.put writeBuffer (size a) (put a)

defaultBufSize :: Int
defaultBufSize = 32 * 1024

-- | Write buffered docs to disk.
writeDocuments :: SegmentId
               -> FieldOrds
               -> Seq Document
               -> IDir ()
writeDocuments segmentId fields documents = do
  withAppendFile (fieldIndexFile segmentId) $ \fdxFile ->
    withAppendFile (fieldDataFile segmentId) $ \fdtFile -> liftIO $ do

    let
      fdxFlush = Files.append fdxFile
      fdtFlush = Files.append fdtFile

    Writer.withWriteBuffer defaultBufSize fdxFlush $ \fdxBuf ->
      Writer.withWriteBuffer defaultBufSize fdtFlush $ \fdtBuf -> do

      for_ documents $ \document -> do
        pos <- Writer.offset fdtBuf
        write_ fdxBuf word64 (fromIntegral pos)

        forFields_ fields $ \fieldOrd fieldName ->
          case HashMap.lookup fieldName (docFields document) of
            Just docField | dfType docField /= FT_Null
              -> write_ fdtBuf (varint >*< fieldValueWrite)
                  (fieldOrd, dfValue docField)
            _ -> return ()
      return ()

  return ()
  where
    write_ :: Writer.WriteBuffer -> Write a -> a -> IO ()
    write_ writeBuffer (W size put) a =
      Writer.put writeBuffer (size a) (put a)

-- | Atomically creates an @AppendFile@ in the @IndexDirectory@.
-- Files are created atomically, if an exception occurrs the
-- created file is destroyed.
withAppendFile :: IDir FilePath -> (AppendFile -> IDir a) -> IDir a
withAppendFile mkPath action = IDir $ \k indexDirectory ->
  let
    withPath path =
      let
        tmpPath  = path <.> "tmp"
        finish a = renamePath tmpPath path >> k a
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
  withIndexRoot $ \root -> root </> segmentIdToBase36 segmentId <.> "tv"

occurrenceFile :: SegmentId -> IDir FilePath
occurrenceFile segmentId =
  withIndexRoot $ \root -> root </> segmentIdToBase36 segmentId <.> "occs"

positionFile :: SegmentId -> IDir FilePath
positionFile segmentId =
  withIndexRoot $ \root -> root </> segmentIdToBase36 segmentId <.> "pos"

fieldIndexFile :: SegmentId -> IDir FilePath
fieldIndexFile segmentId =
  withIndexRoot $ \root -> root </> segmentIdToBase36 segmentId <.> "fdx"

fieldDataFile :: SegmentId -> IDir FilePath
fieldDataFile segmentId =
  withIndexRoot $ \root -> root </> segmentIdToBase36 segmentId <.> "fdt"
