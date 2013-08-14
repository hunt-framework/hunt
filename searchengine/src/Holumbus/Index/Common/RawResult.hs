{-# OPTIONS #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common.RawResult
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  The RawResult data type

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common.RawResult
where

import           Data.Map                          (Map)
import qualified Data.Map                          as M

import           Holumbus.Index.Common.BasicTypes
import           Holumbus.Index.Common.DocIdMap    (DocIdMap)
import qualified Holumbus.Index.Common.DocIdMap    as DM
import           Holumbus.Index.Common.Occurrences

-- ------------------------------------------------------------

-- | The raw result returned when searching the index.
type RawResult    = [(Word, Occurrences)]

-- | Transform the raw result into a tree structure ordered by word.
resultByWord      :: Context -> RawResult -> Map Word (Map Context Occurrences)
resultByWord c
    = M.fromList . map (\ (w, o) -> (w, M.singleton c o))

-- | Transform the raw result into a tree structure ordered by document.
resultByDocument  :: Context -> RawResult -> DocIdMap (Map Context (Map Word Positions))
resultByDocument c os
    = DM.map transform $
      DM.unionsWith (flip $ (:) . head) (map insertWords os)
    where
      insertWords (w, o) = DM.map (\p -> [(w, p)]) o
      transform w        = M.singleton c (M.fromList w)

-- ------------------------------------------------------------

