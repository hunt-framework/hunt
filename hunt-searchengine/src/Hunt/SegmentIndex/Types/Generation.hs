module Hunt.SegmentIndex.Types.Generation where

newtype Generation = Generation Int
                   deriving (Eq, Ord)

instance Show Generation where
  show (Generation g) = show g

generationZero :: Generation
generationZero = Generation 0

nextGeneration :: Generation -> Generation
nextGeneration (Generation g) = Generation (g + 1)
