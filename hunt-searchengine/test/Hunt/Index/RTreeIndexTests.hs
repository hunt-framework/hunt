module Hunt.Index.RTreeIndexTests
(rTreeIndexTests)
where

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Data.RTree.MBB                 as RTree
import           Hunt.Common.Occurrences

import qualified Hunt.Index                     as Ix
import qualified Hunt.Index.RTreeIndex          as PIx

import           Hunt.TestHelper                ()
import           Hunt.Index.TestHelper

rTreeIndexTests :: [Test]
rTreeIndexTests = [
    testProperty "RTree:                   insert"        test_insert
  , testProperty "RTree:                   delete"        test_delete
  , testProperty "RTree:                   merge "        test_merge
  -- TODO:
  -- unionWith
  -- map
  -- mapMaybe
  -- keys
  -- toList
  -- fromList
  ]

mkEmpty :: PIx.RTreeIndex Occurrences
mkEmpty = Ix.empty

mkKey :: RTree.MBB
mkKey = PIx.readPosition "1-1"

test_insert :: Property
test_insert = monadicIO $ do
  occs <- pick arbitrary
  insertTest mkEmpty mkKey occs

test_delete :: Property
test_delete = monadicIO $ do
  occs <- pick arbitrary
  deleteTest mkEmpty mkKey (merge occs occOne) docIdOne

test_merge :: Property
test_merge = monadicIO $ do
  occs1 <- pick arbitrary
  occs2 <- pick arbitrary
  mergeTest mkEmpty mkKey occs1 occs2
