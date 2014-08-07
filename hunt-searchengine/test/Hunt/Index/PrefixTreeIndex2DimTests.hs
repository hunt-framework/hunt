module Hunt.Index.PrefixTreeIndex2DimTests
(prefixTreeIndex2DimTests)
where

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Hunt.Common.Occurrences

import qualified Hunt.Index                     as Ix
import qualified Hunt.Index.PrefixTreeIndex2Dim as PIx

import           Hunt.TestHelper                ()
import           Hunt.Index.TestHelper

prefixTreeIndex2DimTests :: [Test]
prefixTreeIndex2DimTests = [
    testProperty "DmPrefixTree2Dim:        insert"        test_insert
  , testProperty "DmPrefixTree2Dim:        delete"        test_delete
  , testProperty "DmPrefixTree2Dim:        merge "        test_merge
  -- TODO:
  -- unionWith
  -- map
  -- mapMaybe
  -- keys
  -- toList
  -- fromList
  ]

mkEmpty :: PIx.DmPrefixTree Occurrences
mkEmpty = Ix.empty

test_insert :: Property
test_insert = monadicIO $ do
  occs <- pick arbitrary
  insertTest mkEmpty "test" occs

test_delete :: Property
test_delete = monadicIO $ do
  occs <- pick arbitrary
  deleteTest mkEmpty "test" (merge occs occOne) docIdOne

test_merge :: Property
test_merge = monadicIO $ do
  occs1 <- pick arbitrary
  occs2 <- pick arbitrary
  mergeTest mkEmpty "test" occs1 occs2
