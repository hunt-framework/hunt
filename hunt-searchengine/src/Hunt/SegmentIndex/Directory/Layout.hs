module Hunt.SegmentIndex.Directory.Layout where

import           Hunt.SegmentIndex.Types.Generation
import           Hunt.SegmentIndex.Types.SegmentId

import           System.FilePath

termVectorFile :: SegmentId -> FilePath
termVectorFile sid = show sid <.> "tv"

occurrencesFile :: SegmentId -> FilePath
occurrencesFile sid = show sid <.> "occ"

positionsFile :: SegmentId -> FilePath
positionsFile sid = show sid <.> "pos"

fieldInfoFile :: SegmentId -> FilePath
fieldInfoFile sid = show sid <.> "fnm"

fieldIndexFile :: SegmentId -> FilePath
fieldIndexFile sid = show sid <.> "fdx"

fieldDataFile :: SegmentId -> FilePath
fieldDataFile sid = show sid <.> "fdt"

segmentInfosFile :: Generation -> FilePath
segmentInfosFile gen = "gen_" ++ show gen
