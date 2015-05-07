module Hunt.ContextIndex.Snapshot.Files(
    listSnapshotFiles
  , IxFileSet(..)
  ) where

import           Hunt.ContextIndex.Types

import           Control.Applicative
import           Control.Monad
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified System.Directory as Directory
import qualified System.FilePath as FilePath
import           Text.Parsec (parse)
import qualified Text.Parsec as Parsec

data IxFileType
  = IxFileDeletedContexts
  | IxFileDeletedDocs
  | IxFileTerms
  deriving (Eq, Ord, Show)

data IxFile
  = IxFile { ixFileType   :: !IxFileType
           , ixSnapshotId :: !SnapshotId
           , ixFilePath   :: !FilePath
           }
           deriving (Eq, Show)

data IxFileSet
  = IxFileSet { ixfSnapshotId      :: !SnapshotId
              , ixfTerms           :: !FilePath
              , ixfDeletedContexts :: !(Maybe FilePath)
              , ixfDeletedDocs     :: !(Maybe FilePath)
              }
              deriving (Eq, Show)

listSnapshotFiles :: FilePath -> IO [IxFileSet]
listSnapshotFiles fp
  = do files <- Directory.getDirectoryContents fp
       let files'
             = List.concatMap (maybeToList . parseIxFilePath) files
       return (listSnapshotFiles' files')

listSnapshotFiles' :: [IxFile] -> [IxFileSet]
listSnapshotFiles' fx
  = do (sid, files) <- Map.toDescList grouped
       let termFile    = Map.lookup IxFileTerms files
           delCxFile   = Map.lookup IxFileDeletedContexts files
           delDocsFile = Map.lookup IxFileDeletedDocs files
       case termFile of
        Just terms -> return (IxFileSet sid terms delCxFile delDocsFile)
        _          -> mzero
  where
    grouped :: Map SnapshotId (Map IxFileType FilePath)
    grouped
      = Map.fromListWith Map.union
        [ (ixSnapshotId ixf, Map.singleton (ixFileType ixf) (ixFilePath ixf) ) | ixf <- fx ]

parseIxFilePath :: FilePath -> Maybe IxFile
parseIxFilePath fp
  = do sid <- either (const Nothing) Just (parse parser "" file')
       t   <- case ext of
             ".cx.del"  -> pure IxFileDeletedContexts
             ".doc.del" -> pure IxFileDeletedDocs
             ".terms"   -> pure IxFileTerms
             _          -> mzero
       return (IxFile t sid fp)
  where
    (_, file)
      = FilePath.splitFileName fp
    file'
      = FilePath.dropExtension file
    ext
      = FilePath.takeExtension file
    parseSnapshotId
      = (SnapshotId . read) <$> Parsec.many1 Parsec.digit
    parser
      = Parsec.string "snapshot-" *> parseSnapshotId <* Parsec.eof
