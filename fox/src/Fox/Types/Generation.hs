module Fox.Types.Generation where

newtype Generation = Generation Int
                   deriving (Eq, Ord, Show)

firstGeneration :: Generation
firstGeneration = Generation 1

nextGeneration :: Generation -> Generation
nextGeneration (Generation g) = Generation (g + 1)

