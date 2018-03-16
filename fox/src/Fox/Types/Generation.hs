{-# LANGUAGE DeriveGeneric #-}
module Fox.Types.Generation where

import GHC.Generics (Generic)

newtype Generation = Generation Int
                   deriving (Eq, Ord, Show, Generic)

genesis :: Generation
genesis = Generation 0

nextGeneration :: Generation -> Generation
nextGeneration (Generation g) = Generation (g + 1)

pretty :: Generation -> String
pretty (Generation g) = show g

fromInt :: Int -> Generation
fromInt = Generation
