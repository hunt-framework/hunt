{-# LANGUAGE RankNTypes #-}
module Fox.Index.Directory (
    IndexDirectory
  , defaultIndexDirectory
  , createIndexDirectory

  , SegmentDirLayout(..)
  , segmentDirLayout
  ) where

import qualified Fox.Types.SegmentId as SegmentId

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
