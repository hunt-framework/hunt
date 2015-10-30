module Main where

import           Data.Foldable
import qualified Data.IntMap.Packed        as IntMapP
import qualified Data.IntMap.Strict        as IntMap
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

unionProp0 :: IntMap.IntMap Int -> IntMap.IntMap Int -> Bool
unionProp0 im1 im2 =
  IntMap.toList (im1 `IntMap.union` im2) == IntMapP.toList (im1' `IntMapP.union` im2')
  where
    im1' = IntMapP.fromList (IntMap.toList im1)
    im2' = IntMapP.fromList (IntMap.toList im2)

unionProp1 :: IntMap.IntMap Int -> IntMap.IntMap Int -> Bool
unionProp1 im1 im2 =
  IntMap.toList (IntMap.unionWith (+) im1 im2) == IntMapP.toList (IntMapP.unionWith (+) im1' im2')
  where
    im1' = IntMapP.fromList (IntMap.toList im1)
    im2' = IntMapP.fromList (IntMap.toList im2)

interProp0 :: IntMap.IntMap Int -> IntMap.IntMap Int -> Bool
interProp0 im1 im2 =
  IntMap.toList (im1 `IntMap.intersection` im2) == IntMapP.toList (im1' `IntMapP.intersection` im2')
  where
    im1' = IntMapP.fromList (IntMap.toList im1)
    im2' = IntMapP.fromList (IntMap.toList im2)

interProp1 :: IntMap.IntMap Int -> IntMap.IntMap Int -> Bool
interProp1 im1 im2 =
  IntMap.toList (IntMap.intersectionWith (+) im1 im2) == IntMapP.toList (IntMapP.intersectionWith (+) im1' im2')
  where
    im1' = IntMapP.fromList (IntMap.toList im1)
    im2' = IntMapP.fromList (IntMap.toList im2)

diffProp0 :: IntMap.IntMap Int -> IntMap.IntMap Int -> Bool
diffProp0 im1 im2 =
  IntMap.toList (im1 `IntMap.difference` im2) == IntMapP.toList (im1' `IntMapP.difference` im2')
  where
    im1' = IntMapP.fromList (IntMap.toList im1)
    im2' = IntMapP.fromList (IntMap.toList im2)

diffProp1 :: IntMap.IntMap Int -> IntMap.IntMap Int -> Bool
diffProp1 im1 im2 =
  IntMap.toList (IntMap.differenceWith (\x y -> Just (x + y)) im1 im2) == IntMapP.toList (IntMapP.differenceWith (\x y -> Just (x + y)) im1' im2')
  where
    im1' = IntMapP.fromList (IntMap.toList im1)
    im2' = IntMapP.fromList (IntMap.toList im2)

main :: IO ()
main = for_ [ unionProp0
            , unionProp1
            , interProp0
            , interProp1
            , diffProp0
            , diffProp1
            ] (quickCheckWith (stdArgs { maxSize = 10000
                                       , maxDiscardRatio = 10000
                                       }))
