{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Hunt.SegmentIndex.Types.Generation where

import           Data.Binary (Binary)
import           Data.Store

newtype Generation = Generation Int
                   deriving (Store, Binary, Eq, Ord)

instance Show Generation where
  show (Generation g) = show g

instance Read Generation where
  readsPrec x s = case readsPrec x s of
                    [(g, "")] -> [(Generation g, "")]
                    _         -> []

generationZero :: Generation
generationZero = Generation 0

nextGeneration :: Generation -> Generation
nextGeneration (Generation g) = Generation (g + 1)
