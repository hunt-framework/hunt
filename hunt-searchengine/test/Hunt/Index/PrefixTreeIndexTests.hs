module Hunt.Index.PrefixTreeIndexTests
(prefixTreeIndexTests)
where

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Hunt.Common.Occurrences
import           Hunt.Common.Positions          (Positions)

import qualified Hunt.Index                     as Ix
import qualified Hunt.Index.PrefixTreeIndex     as PIx

import           Hunt.TestHelper                ()
import           Hunt.Index.TestHelper

prefixTreeIndexTests :: [Test]
prefixTreeIndexTests = [
    testProperty "DmPrefixTree:            insert"        test_insert
  , testProperty "DmPrefixTree:            delete"        test_delete
  , testProperty "DmPrefixTree:            merge"         test_merge
  -- TODO:
  -- unionWith
  -- map
  -- mapMaybe
  -- keys
  -- toList
  -- fromList
  ]

mkEmpty :: PIx.DmPrefixTree Positions
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
