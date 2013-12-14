{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.DeepSeq
import           Data.Map                             (Map)

import           Test.Framework
--import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
--import           Test.HUnit
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Monadic              (assert, monadicIO, run, pick)


import qualified Data.Map                             as M
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Holumbus.Common
import qualified Holumbus.Common.DocIdMap             as DM
--import           Holumbus.Common.Occurrences         (singleton)


import qualified Holumbus.Index.Index                 as Ix
import qualified Holumbus.Index.PrefixTreeIndex       as PIx
import qualified Holumbus.Index.ComprPrefixTreeIndex  as CPIx
import qualified Holumbus.Index.InvertedIndex         as InvIx

import           GHC.AssertNF

-- ----------------------------------------------------------------------------
main :: IO ()
main = do
--  assertNF $! y
--  bool <- isNF $! y
--  putStrLn $ show bool
  defaultMain [ testProperty "prop_range" prop_ptix ]


-- ----------------------------------------------------------------------------
-- test with simple index 
-- ----------------------------------------------------------------------------

prop_ptix :: Property
prop_ptix = monadicIO $ do
                          x <- pick arbitrary 
                          passed <- run $ isNF $! ix x
                          assert passed
              where
              ix :: Int -> PIx.DmPrefixTree (Tuple Int)
              ix x =  Ix.insert "key" (DM.singleton 1 (mkTuple x)) Ix.empty



-- ----------------------------------------------------------------------------
-- test property
-- ----------------------------------------------------------------------------
prop_simple :: Property
prop_simple = monadicIO $ do
                          x <- pick arbitrary 
                          passed <- run $ isNF $! mkTuple x
                          assert passed

inc :: Int -> Int
inc x = 1 + x

data Tuple x = Tuple {
  val1 :: !x,
  val2 :: !x
} deriving (Eq, Show)

instance NFData (Tuple x) where

mkTuple :: Int -> Tuple Int
mkTuple x = Tuple (inc x) (inc x)


--instance Arbitrary Text where
--    arbitrary = T.pack <$> arbitrary
--    shrink xs = T.pack <$> shrink (T.unpack xs)

--test2 :: Assertion
--test2 = assertNF $ ptIndex

-- --------------------
-- Arbitrary Occurrences

-- XX TODO

-- --------------------
-- Arbitrary ApiDocument

apiDocs :: Int -> Int -> IO [ApiDocument]
apiDocs = mkData apiDocGen


mkData :: (Int -> Gen a) -> Int -> Int -> IO [a]
mkData gen minS maxS =
  do rnd0 <- newStdGen
     let rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = split rnd
     return [unGen (gen i) r n | ((r,n),i) <- rnds rnd0 `zip` cycle [minS..maxS] `zip` [1..]] -- simple cycle


apiDocGen :: Int -> Gen ApiDocument
apiDocGen n = do
  desc_    <- descriptionGen
  let ix  =  mkIndexData n desc_
  return  $ ApiDocument uri_ ix desc_
  where uri_ = T.pack . ("rnd://" ++) . show $ n

niceText1 :: Gen Text
niceText1 = fmap T.pack . listOf1 . elements $ concat [" ", ['0'..'9'], ['A'..'Z'], ['a'..'z']]


descriptionGen :: Gen Description
descriptionGen = do
  tuples <- listOf kvTuples
  return $ M.fromList tuples
  where
  kvTuples = do
    a <- resize 15 niceText1 -- keys are short
    b <- niceText1
    return (a,b)


mkIndexData :: Int -> Description -> Map Context Content
mkIndexData i d = M.fromList
                $ map (\c -> ("context" `T.append` (T.pack $ show c), prefixx c)) [0..i]
  where
--  index   = T.pack $ show i
  prefixx n = T.intercalate " " . map (T.take n . T.filter (/=' ') . snd) . M.toList $ d

-- ------------------------------------------------------------

