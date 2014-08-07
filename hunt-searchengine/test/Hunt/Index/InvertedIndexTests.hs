module Hunt.Index.InvertedIndexTests
(invertedIndexTests)
where

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.Providers.HUnit
import           Test.HUnit                    hiding (Test)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Hunt.Common.BasicTypes
import qualified Hunt.Common.DocIdSet           as DS
import           Hunt.Common.IntermediateValue

import qualified Hunt.Index                     as Ix
import qualified Hunt.Index.InvertedIndex       as InvIx


import           Hunt.TestHelper                ()
import           Hunt.Index.TestHelper

invertedIndexTests :: [Test]
invertedIndexTests = [
    testProperty "InvertedIndex:           insert"        test_insert
  , testCase     "InvertedIndex:           delete"        test_delete
  , testProperty "InvertedIndex:           merge "        test_merge
  -- TODO:
  -- unionWith
  -- map
  -- mapMaybe
  -- keys
  -- toList
  -- fromList
  ]

mkEmpty :: InvIx.InvertedIndex
mkEmpty = Ix.empty

mkKey :: Word
mkKey = "test"

-- | check deleteDocs with InvertedIndex
test_delete :: Assertion
test_delete = do
  let occ = occOne
  ix <- Ix.insertM mkKey (toIntermediate occ) mkEmpty
  [(_,nv)] <- Ix.searchM PrefixNoCase mkKey ix
  (fromIntermediate nv) @?= occ
  let delNot = DS.fromList [docIdTwo]
  ix' <- Ix.deleteDocsM delNot ix
  [(_,nv')] <- Ix.searchM PrefixNoCase mkKey ix'
  (fromIntermediate nv') @?= occ
  let delReal = DS.fromList [docIdOne]
  ix'' <- Ix.deleteDocsM delReal ix'
  result <- Ix.searchM PrefixNoCase mkKey ix''
  result @?= []

test_insert :: Property
test_insert = monadicIO $ do
  occs <- pick arbitrary
  insertTest mkEmpty mkKey occs

test_merge :: Property
test_merge = monadicIO $ do
  occs1 <- pick arbitrary
  occs2 <- pick arbitrary
  mergeTest mkEmpty mkKey occs1 occs2
