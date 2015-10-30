module Main where

import           Data.Foldable
import qualified Data.IntSet.Packed        as IntSetP
import qualified Data.IntSet               as IntSet
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

member0 :: IntSet.IntSet ->  IntSet.IntSet -> Bool
member0 _ is1 = all (\x -> IntSetP.member x is1') (IntSet.toList is1)
  where
    is1' = IntSetP.fromList (IntSet.toList is1)

unionProp0 :: IntSet.IntSet -> IntSet.IntSet -> Bool
unionProp0 im1 im2 =
  IntSet.toList (im1 `IntSet.union` im2) == IntSetP.toList (im1' `IntSetP.union` im2')
  where
    im1' = IntSetP.fromList (IntSet.toList im1)
    im2' = IntSetP.fromList (IntSet.toList im2)

interProp0 :: IntSet.IntSet -> IntSet.IntSet -> Bool
interProp0 im1 im2 =
  IntSet.toList (im1 `IntSet.intersection` im2) == IntSetP.toList (im1' `IntSetP.intersection` im2')
  where
    im1' = IntSetP.fromList (IntSet.toList im1)
    im2' = IntSetP.fromList (IntSet.toList im2)

diffProp0 :: IntSet.IntSet -> IntSet.IntSet -> Bool
diffProp0 im1 im2 =
  IntSet.toList (im1 `IntSet.difference` im2) == IntSetP.toList (im1' `IntSetP.difference` im2')
  where
    im1' = IntSetP.fromList (IntSet.toList im1)
    im2' = IntSetP.fromList (IntSet.toList im2)

main :: IO ()
main = for_ [ member0
            , unionProp0
            , interProp0
            , diffProp0
            ] (quickCheckWith (stdArgs { maxSize = 10000
                                       , maxDiscardRatio = 10000
                                       }))
