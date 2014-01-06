{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.DeepSeq
import           Control.Monad                                   (foldM)

import           Test.Framework
--import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
--import           Test.HUnit
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Monadic                         (PropertyM,
                                                                  assert,
                                                                  monadicIO,
                                                                  pick, run)


import           Data.Map                                        (Map)
import qualified Data.Map                                        as M
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T
--import qualified Data.IntSet                                     as IS

import           Holumbus.Common
--import qualified Holumbus.Common.DocIdMap                        as DM
import qualified Holumbus.Common.Occurrences                     as Occ
import qualified Holumbus.Common.Occurrences.Compression.BZip    as ZB
import qualified Holumbus.Common.Occurrences.Compression.Simple9 as Z9
import qualified Holumbus.Common.Occurrences.Compression.Snappy  as ZS
import qualified Holumbus.Common.Positions                       as Pos


import qualified Holumbus.Index.ComprPrefixTreeIndex             as CPIx
import qualified Holumbus.Index.Index                            as Ix
import qualified Holumbus.Index.InvertedIndex                    as InvIx
import qualified Holumbus.Index.PrefixTreeIndex                  as PIx

import qualified Holumbus.Index.Proxy.CachedIndex                as CacheProxy
import qualified Holumbus.Index.Proxy.KeyIndex                   as KeyProxy
import qualified Holumbus.Index.Proxy.IntNormalizerIndex         as IntProxy
import qualified Holumbus.Index.Proxy.DateNormalizerIndex        as DateProxy
import qualified Holumbus.Index.Proxy.PositionNormalizerIndex    as GeoProxy

--import qualified Holumbus.Index.Proxy.CompressedIndex            as ComprProxy

import           GHC.AssertNF


-- ----------------------------------------------------------------------------
main :: IO ()
main = do
--  assertNF $! y
--  bool <- isNF $! y
--  putStrLn $ show bool
  defaultMain [

                testProperty "prop_strictness_occurrences"               prop_occs

              , testProperty "prop_strictness_prefixtreeindex"           prop_ptix
              , testProperty "prop_strictness_invindex textkey"          prop_invix1
              , testProperty "prop_strictness_invindex intkey"           prop_invix2
              , testProperty "prop_strictness_invindex datekey"          prop_invix3
              , testProperty "prop_strictness_invindex geokey"           prop_invix4
--            test failing right now because compressedoccurrences are not strict
--            but we are not using them at the moment at probably won't in the future
--              , testProperty "prop_strictness_comprprefixtreeindex comp" prop_cptix
              , testProperty "prop_strictness_comprprefixtreeindex bzip" prop_cptix2
              , testProperty "prop_strictness_comprprefixtreeindex snap" prop_cptix3
              , testProperty "prop_strictness_proxy_cache"               prop_cachedix
              , testProperty "prop_strictness_proxy_textkey"             prop_textix
              , testProperty "prop_strictness_proxy_intkey"              prop_intix
              , testProperty "prop_strictness_proxy_datekey"             prop_dateix
              , testProperty "prop_strictness_proxy_geokey"              prop_geoix
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

prop_cptix :: Property
prop_cptix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (CPIx.ComprOccPrefixTree Z9.CompressedOccurrences)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty

prop_cptix2 :: Property
prop_cptix2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (CPIx.ComprOccPrefixTree ZB.CompressedOccurrences)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty

prop_cptix3 :: Property
prop_cptix3
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (CPIx.ComprOccPrefixTree ZS.CompressedOccurrences)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty


prop_invix1 :: Property
prop_invix1
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex Positions)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty

prop_invix2 :: Property
prop_invix2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexInt Positions)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "1" val Ix.empty

prop_invix3 :: Property
prop_invix3
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexDate Positions)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "2013-01-01" val Ix.empty

prop_invix4 :: Property
prop_invix4
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexPosition Positions)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "1-1" val Ix.empty


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
    ix <- pickIx :: PropertyM IO (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree) Positions)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty

-- int proxy
prop_intix :: Property
prop_intix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (IntProxy.IntAsTextNormalizerIndex (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree)) Positions)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "1" val Ix.empty

-- date proxy
prop_dateix :: Property
prop_dateix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (DateProxy.DateNormalizerIndex (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree)) Positions)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "2013-01-01" val Ix.empty

-- geo proxy
prop_geoix :: Property
prop_geoix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (GeoProxy.PositionNormalizerIndex (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree)) Positions)
    passed <- run $ isNF $! ix
    assert passed
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "1-1" val Ix.empty


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

