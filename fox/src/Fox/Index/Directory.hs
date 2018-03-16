{-# LANGUAGE RankNTypes #-}
module Fox.Index.Directory (
    IndexDirectory
  , defaultIndexDirectory
  , createIndexDirectory

  , listMetaFiles

  , SegmentDirLayout(..)
  , segmentDirLayout

  , MetaDirLayout(..)
  , metaDirLayout
  ) where

import qualified Fox.Types.SegmentId as SegmentId
import qualified Fox.Types.Generation as Generation

import qualified Data.List as List
import qualified Data.Ord as Ord
import System.FilePath ((</>), (<.>))
import qualified System.FilePath as FilePath
import qualified System.Directory as Directory
import qualified Text.Read as Read


-- | Root directory where the index files are stored.
newtype IndexDirectory = IndexDirectory { _unIndexDirectory :: FilePath }
                       deriving (Eq, Ord, Show)

defaultIndexDirectory :: IndexDirectory
defaultIndexDirectory = IndexDirectory "index"

createIndexDirectory :: IndexDirectory -> IO ()
createIndexDirectory (IndexDirectory indexDir) =
  Directory.createDirectoryIfMissing True indexDir

listIndexDirectory :: IndexDirectory -> IO [FilePath]
listIndexDirectory (IndexDirectory indexDir) =
  Directory.listDirectory indexDir

-- | List all the index meta files in the 'IndexDirectory'.
-- Returns the meta file pathes in descending order of
-- generations.
listMetaFiles :: IndexDirectory -> IO [(Generation.Generation, FilePath)]
listMetaFiles indexDir@(IndexDirectory ixDir) = do
  files <- listIndexDirectory indexDir

  let
    toMetaFile fp
      | "meta"   <- FilePath.takeBaseName fp
      , '.':rest <- FilePath.takeExtension fp
      , Just gen <- Read.readMaybe rest
      = [ (Generation.fromInt gen, ixDir </> fp) ]
      | otherwise
      = []

    metaFiles =
      (List.concatMap toMetaFile files)

  return (List.sortOn (Ord.Down . fst) metaFiles)


data MetaDirLayout
  = MetaDirLayout {
        metaMetaFile :: Generation.Generation -> FilePath
      }

metaDirLayout :: IndexDirectory -> MetaDirLayout
metaDirLayout (IndexDirectory indexDir) =
  let
    metaFile generation =
      indexDir </> "meta" <.> Generation.pretty generation
  in
    MetaDirLayout {
      metaMetaFile = metaFile
    }

data SegmentDirLayout
  = SegmentDirLayout {
      segmentVocabularyFile  :: FilePath
    , segmentOccurrencesFile :: FilePath
    , segmentPostingsFile    :: FilePath
    , segmentIxFile          :: FilePath
    }

segmentDirLayout
  :: IndexDirectory
  -> SegmentId.SegmentId
  -> SegmentDirLayout
segmentDirLayout (IndexDirectory indexDir) segmentId =
  let
    segmentBase = indexDir </> SegmentId.segmentIdToBase36 segmentId
  in
    SegmentDirLayout {
      segmentVocabularyFile  = segmentBase <.> "voc"
    , segmentOccurrencesFile = segmentBase <.> "occ"
    , segmentPostingsFile    = segmentBase <.> "pos"
    , segmentIxFile          = segmentBase <.> "ix"
    }
