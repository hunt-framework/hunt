{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Monad                        (foldM)
import           Control.DeepSeq
import           Data.Map                             (Map)

import           Test.Framework
--import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
--import           Test.HUnit
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Monadic              (PropertyM, assert, monadicIO, run, pick)


import qualified Data.Map                             as M
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Data.IntSet                          as IS

import           Holumbus.Common
import qualified Holumbus.Common.DocIdMap             as DM
import qualified Holumbus.Common.Positions            as Pos
import qualified Holumbus.Common.Occurrences          as Occ


import qualified Holumbus.Index.Index                 as Ix
import qualified Holumbus.Index.PrefixTreeIndex       as PIx
import qualified Holumbus.Index.ComprPrefixTreeIndex  as CPIx
import qualified Holumbus.Index.InvertedIndex         as InvIx

import qualified Holumbus.Index.Proxy.CachedIndex     as CacheProxy
import qualified Holumbus.Index.Proxy.TextKeyIndex    as TextProxy
import qualified Holumbus.Index.Proxy.CompressedIndex as ComprProxy

import           GHC.AssertNF

-- ----------------------------------------------------------------------------
main :: IO ()
main = do
--  assertNF $! y
--  bool <- isNF $! y
--  putStrLn $ show bool
  defaultMain [

                testProperty "prop_strictness_occurrences"          prop_occs

              , testProperty "prop_strictness_prefixtreeindex"      prop_ptix
              , testProperty "prop_strictness_comprprefixtreeindex" prop_cptix
              , testProperty "prop_strictness_invindex"             prop_invix
              , testProperty "prop_strictness_proxy_cache"          prop_cachedix

              ]

-- ----------------------------------------------------------------------------
-- test data structures
-- ----------------------------------------------------------------------------

prop_occs :: Property
prop_occs = monadicIO $ do
              x <- pick arbitrary :: PropertyM IO Occurrences
              -- $!! needed here - $! does not evaluate everything of course
              passed <- run $ isNF $!! x
              assert passed

-- ----------------------------------------------------------------------------
-- test with simple index
-- ----------------------------------------------------------------------------

-- | helper generating random indices


prop_ptix :: Property
prop_ptix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.DmPrefixTree Positions)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty

-- why is this in NF - we don't call the constructor in the implementation..?
prop_cptix :: Property
prop_cptix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (CPIx.ComprOccPrefixTree CompressedPositions)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty


-- this is failing atm - not implemented seq here
prop_invix :: Property
prop_invix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex Positions)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty


-- ----------------------------------------------------------------------------
-- test index proxies
-- ----------------------------------------------------------------------------

-- cache
prop_cachedix :: Property
prop_cachedix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (CacheProxy.CachedIndex (PIx.DmPrefixTree) Positions)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty


-- text proxy
prop_textix :: Property
prop_textix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (TextProxy.TextKeyProxyIndex (PIx.DmPrefixTree) Positions)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty


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

instance Arbitrary Occurrences where
  arbitrary = mkOccurrences

mkOccurrences :: Gen Occurrences
mkOccurrences = listOf mkPositions >>= foldM foldOccs Occ.empty
  where
  foldOccs occs ps = do
    docId <- arbitrary
    return $ Occ.insert' docId ps occs

mkPositions :: Gen Positions
mkPositions = listOf arbitrary >>= return . Pos.fromList

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

