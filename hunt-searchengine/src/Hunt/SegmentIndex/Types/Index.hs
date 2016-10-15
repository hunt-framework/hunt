module Hunt.SegmentIndex.Types.Index where

import           Hunt.Common.BasicTypes
import           Hunt.Scoring.Score
import           Hunt.SegmentIndex.Types.TermInfo

-- | The index type which needs to be implemented to be used by the 'Interpreter'.
--   The implementation must have a value type parameter.
data Index a =
  Index { ixSearch   :: TextSearchOp -> a -> IO [(a, TermInfo)]
          -- ^ General lookup function.
        , ixSearchSc :: TextSearchOp -> a -> IO [(a, (Score, TermInfo))]
          -- ^ Search with a scoring of the result by comparing the search key
          -- with the key in the result and estimating the similarity of these
          -- keys.
          --
          -- The default implementation is attaching always the default score (1.0)
        , ixLookupRange :: a -> a -> IO [(a, TermInfo)]
          -- ^ Search within a range of two keys.
        , ixLookupRangeSc :: a -> a -> IO [(a, (Score, TermInfo))]

          -- ^ Search withinin a range and scoring of the result by
          -- comparing the keys of the bounds with the key in the result
          -- and estimating the similarity of these keys.
          --
          -- The default implementation is attaching always the default score (1.0)
        }
