{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import           Control.DeepSeq
import           Control.Monad                                   (foldM)

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Monadic                         (PropertyM,
                                                                  assert,
                                                                  monadicIO,
                                                                  monitor,
                                                                  pick, run)

import           Data.Map                                        (Map)
import qualified Data.Map                                        as M
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T

import           GHC.AssertNF
import           GHC.HeapView
import qualified System.Mem


import           Hunt.Common
import qualified Hunt.Common.Positions                       as Pos
import qualified Hunt.Common.Occurrences                     as Occ

import qualified Hunt.Index                                  as Ix
import qualified Hunt.Index.InvertedIndex                    as InvIx
import qualified Hunt.Index.PrefixTreeIndex                  as PIx
import           Hunt.Index.IndexImpl

import qualified Hunt.Index.Proxy.KeyIndex                   as KeyProxy

-- ----------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  -- strictness property for data-structures used in index and
  -- document table
  [ testProperty "prop_strictness_occurrences"               prop_occs
  , testProperty "prop_strictness_document"                  prop_doc
  , testProperty "prop_strictness_description"               prop_desc 

  -- strictness property for index implementations
  , testProperty "prop_strictness_prefixtreeindex"           prop_ptix
  , testProperty "prop_strictness_invindex textkey"          prop_invix1
  , testProperty "prop_strictness_invindex intkey"           prop_invix2
  , testProperty "prop_strictness_invindex datekey"          prop_invix3
  , testProperty "prop_strictness_invindex geokey"           prop_invix4

  -- strictness property for document table

  ]

-- ----------------------------------------------------------------------------
-- test data structures
-- ----------------------------------------------------------------------------

prop_occs :: Property
prop_occs = monadicIO $ do
  x <- pick arbitrary :: PropertyM IO Occurrences
  assertNF' $! x

prop_doc :: Property
prop_doc = monadicIO $ do
  x <- pick arbitrary :: PropertyM IO Document
  assertNF' $! x

prop_desc :: Property
prop_desc = monadicIO $ do
  x <- pick mkDescription 
  assertNF' $! x

-- ----------------------------------------------------------------------------
-- test with simple index
-- ----------------------------------------------------------------------------

-- | helper generating random indices


prop_ptix :: Property
prop_ptix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.DmPrefixTree Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty

{--
prop_cptix :: Property
prop_cptix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (CPIx.ComprOccPrefixTree Z9.CompressedOccurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty

prop_cptix2 :: Property
prop_cptix2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (CPIx.ComprOccPrefixTree ZB.CompressedOccurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty

prop_cptix3 :: Property
prop_cptix3
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (CPIx.ComprOccPrefixTree ZS.CompressedOccurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty
--}

prop_invix1 :: Property
prop_invix1
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty

prop_invix2 :: Property
prop_invix2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexInt Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "1" val Ix.empty

prop_invix3 :: Property
prop_invix3
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexDate Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "2013-01-01" val Ix.empty

prop_invix4 :: Property
prop_invix4
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexPosition Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "1-1" val Ix.empty


-- ----------------------------------------------------------------------------
-- test index proxies
-- ----------------------------------------------------------------------------

-- cache
{--
prop_cachedix :: Property
prop_cachedix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (CacheProxy.CachedIndex (PIx.DmPrefixTree) Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty

--}
-- text proxy
prop_textix :: Property
prop_textix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree) Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" val Ix.empty

-- int proxy
prop_intix :: Property
prop_intix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexInt Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "1" val Ix.empty

-- date proxy
prop_dateix :: Property
prop_dateix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexDate Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "2013-01-01" val Ix.empty

-- geo proxy
prop_geoix :: Property
prop_geoix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexPosition Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "1-1" val Ix.empty

-- ----------------------------------------------------------------------------
-- test property contextindex
-- ----------------------------------------------------------------------------
{--
prop_contextix_empty :: Property
prop_contextix_empty
  = monadicIO $ do
    assertNF' CIx.empty


prop_contextix_emptyix :: Property
prop_contextix_emptyix
   = monadicIO $ do
    val     <- pick arbitrary :: PropertyM IO Occurrences
    let ix  = Ix.empty :: (InvIx.InvertedIndexPosition Occurrences)
    let ix2 = Ix.empty :: (InvIx.InvertedIndex Occurrences)
    let cix = CIx.insertContext "text" (mkIndex ix2)
            $ CIx.insertContext "geo" (mkIndex ix)
            $ CIx.empty
    assertNF' cix



prop_contextix :: Property
prop_contextix
  = monadicIO $ do
    val     <- pick arbitrary :: PropertyM IO Occurrences
    let ix  = Ix.empty :: (InvIx.InvertedIndexPosition Occurrences)
    let ix2 = Ix.empty :: (InvIx.InvertedIndex Occurrences)
    let cix = CIx.insertWithCx "text" "word" val
            $ CIx.insertContext "text" (mkIndex ix2)
            $ CIx.insertContext "geo" (mkIndex ix)
            $ CIx.empty
    assertNF' cix


prop_contextix2 :: Property
prop_contextix2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexPosition Occurrences)
    ix2 <- pickIx2 :: PropertyM IO (InvIx.InvertedIndex Occurrences)
    let cix = CIx.insertContext "text" (mkIndex ix2)
            $ CIx.insertContext "geo" (mkIndex ix)
            $ CIx.empty
    assertNF' cix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "1-1" val Ix.empty
  pickIx2 = pick arbitrary >>= \val -> return $ Ix.insert "1-1" val Ix.empty
--}
-- ----------------------------------------------------------------------------
-- test property indeximpl
-- ----------------------------------------------------------------------------

{--
prop_impl_full :: Property
prop_impl_full
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexPosition Occurrences)
    assertNF' $ mkIndex ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "1-1" val Ix.empty

prop_impl_empty :: Property
prop_impl_empty
  = monadicIO $ do
    let ix = Ix.empty  :: (InvIx.InvertedIndexPosition Occurrences)
    assertNF' $ mkIndex ix
--}

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

-- --------------------
-- Arbitrary Documents

instance Arbitrary Document where
  arbitrary = mkDocument

mkDocument :: Gen Document
mkDocument = do
  uri <- niceText1
  desc <- mkDescription
  w <- arbitrary
  return $ Document uri desc w 


mkDescription :: Gen Description
mkDescription = do
  txt <- niceText1
  txt2 <- niceText1
  return $ M.fromList [ ("key1", txt)
                      , ("key2", txt2)
                      ]
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
  return  $ ApiDocument uri_ ix desc_ (Just 1.0)
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

heapGraph :: Int -> a -> IO String
heapGraph d x = do
  let box = asBox x
  graph <- buildHeapGraph d () box
  return $ ppHeapGraph graph

isNFWithGraph :: Int -> a -> IO (Bool, String)
isNFWithGraph d x = do
  b <- isNF $! x
  -- XXX: does gc need a delay?
  System.Mem.performGC
  g <- heapGraph d x
  return (b,g)

-- depth is a constant
assertNF' :: a -> PropertyM IO ()
assertNF' = assertNF'' 5

assertNF'' :: Int -> a -> PropertyM IO ()
assertNF'' d x = do
  (b,g) <- run $ isNFWithGraph d x
  monitor $ const $ printTestCase g b

-- ----------------------------------------------------------------------------
