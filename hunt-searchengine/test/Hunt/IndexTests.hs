{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Hunt.IndexTests
(indexImplTests)
where

import qualified Data.Map                       as M

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)

import           Hunt.Common.BasicTypes
import qualified Hunt.Common.DocIdSet           as DS
import           Hunt.Common.Occurrences

import           Hunt.ContextIndex              (addWordsM)
import qualified Hunt.ContextIndex              as ConIx
import qualified Hunt.Index                     as Ix
import           Hunt.Index.IndexImpl
import qualified Hunt.Index.InvertedIndex       as InvIx

import           Hunt.Index.PrefixTreeIndexTests
import           Hunt.Index.TestHelper
-- ----------------------------------------------------------------------------

indexImplTests :: [Test]
indexImplTests =
  prefixTreeIndexTests ++
  [
  -- TODO
  -- REFACTOR THIS INTO MODULS /Hunt/Index/[IndexImpl]Test.hs
  -- to keep each index implementations tests in docIdOne module
  --
  -- test: insertList, deleteDocs, toList, fromList, map
  -- test: intindex, dateindex, geoindex
  -- InvertedIndex implementation
    testCase "InvertedIndex:           insert"        insertTestInvIx
  , testCase "InvertedIndex:           deleteDocs"    deleteDocsTestInvIx
  , testCase "InvertedIndex:           merge"         mergeTestInvIx

  -- ContextIndex implementation
  , testCase "ContextIndex:            insert"        insertTestContextIx
  , testCase "ContextIndex:            insertContext" insertTestContext
  -- helper functions
  , testCase "TextIndex:               addWords"      addWordsTest
  , testCase "Occurrences:             merge"         occMergeTest
  ]

-- ----------------------------------------------------------------------------
-- Check InvertedIndex implementation

-- | check insert InvertedIndex
insertTestInvIx :: Assertion
insertTestInvIx = do
  result <- insertTest
            (Ix.empty::(InvIx.InvertedIndex Occurrences))
            "test" occOne
  True @?= result

-- | check deleteDocs with InvertedIndex
deleteDocsTestInvIx :: Assertion
deleteDocsTestInvIx = do
  let occ = occOne
  ix <- Ix.insertM merge "test" occ (Ix.empty::(InvIx.InvertedIndex Occurrences)) -- the Occurrences type is a dummy in this case
  [(_,nv)] <- Ix.searchM PrefixNoCase "test" ix
  nv @?= occ
  let delNot = DS.fromList [docIdTwo]
  ix' <- Ix.deleteDocsM delNot ix
  [(_,nv')] <- Ix.searchM PrefixNoCase "test" ix'
  nv' @?= occ
  let delReal = DS.fromList [docIdOne]
  ix'' <- Ix.deleteDocsM delReal ix'
  result <- Ix.searchM PrefixNoCase "test" ix''
  result @?= []

-- | test merging of Occurrences in InvertedIndex
mergeTestInvIx :: Assertion
mergeTestInvIx = do
  result <- mergeTest
            (Ix.empty::(InvIx.InvertedIndex Occurrences))
            "test" occOne occTwo
  True @?= result

-- ----------------------------------------------------------------------------
-- Check ContextIndex

-- | check insert context on ContextIndex
insertTestContext :: Assertion
insertTestContext = "test" @?= insertedContext
  where
  [insertedContext] = ConIx.contexts ix
  ix :: ConIx.ContextMap Occurrences
  ix = ConIx.insertContext "test" (mkIndex (Ix.empty :: InvIx.InvertedIndex Occurrences)) ConIx.empty

-- | check insert on ContextIndex
insertTestContextIx :: Assertion
insertTestContextIx
  = do
    let cix = ConIx.insertContext "context" (mkIndex impl) emptyIndex
    cix2 <- ConIx.insertWithCx merge "context" "word" newElem cix
    [(_, insertedElem)] <- ConIx.searchWithCx PrefixNoCase "context" "word" cix2
    True @?= newElem == insertedElem
  where
  newElem = occOne
  emptyIndex :: ConIx.ContextMap Occurrences
  emptyIndex = ConIx.empty
  impl       :: InvIx.InvertedIndex Occurrences
  impl       = Ix.empty


-- ----------------------------------------------------------------------------
-- check helper functions
-- ----------------------------------------------------------------------------

addWordsTest :: Assertion
addWordsTest = do
  resIx   <- addWordsM (wrds "default") docIdOne emptyIndex
  resList <- ConIx.searchWithCx PrefixNoCase "default" "word" $ resIx
  True @?= length resList == 1
  where
  emptyIndex :: ConIx.ContextMap Occurrences
  emptyIndex =  ConIx.insertContext "default" (mkIndex impl) ConIx.empty
  impl       :: InvIx.InvertedIndex Occurrences
  impl       = Ix.empty

occMergeTest :: Assertion
occMergeTest = True @?= (merge occ1 occ2 == occ3)
  where
  occ1 = occOne
  occ2 = occTwo
  occ3 = insert (docIdOne) 2 $ occOne

-- ----------------------------------------------------------------------------
-- test helper
-- ----------------------------------------------------------------------------

wordList :: WordList
wordList = M.fromList $ [("word", [1,5,10])]

wrds :: Word -> Words
wrds w = M.fromList $ [(w, wordList)]
