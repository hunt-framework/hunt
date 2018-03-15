{-# LANGUAGE RankNTypes #-}
module Fox.Index.Directory (
    IndexDirectory
  , defaultIndexDirectory
  , createIndexDirectory

  , SegmentDirLayout(..)
  , segmentDirLayout

  , MetaDirLayout(..)
  , metaDirLayout
  ) where

import qualified Fox.Types.SegmentId as SegmentId
import qualified Fox.Types.Generation as Generation

import System.FilePath ((</>), (<.>))
import qualified System.Directory as Directory


-- | Root directory where the index files are stored.
newtype IndexDirectory = IndexDirectory { _unIndexDirectory :: FilePath }
                       deriving (Eq, Ord, Show)

defaultIndexDirectory :: IndexDirectory
defaultIndexDirectory = IndexDirectory "index"

createIndexDirectory :: IndexDirectory -> IO ()
createIndexDirectory (IndexDirectory indexDir) =
  Directory.createDirectoryIfMissing True indexDir

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
    }
