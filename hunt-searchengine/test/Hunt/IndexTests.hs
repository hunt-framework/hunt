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
import           Hunt.Index.PrefixTreeIndex2DimTests
import           Hunt.Index.RTreeIndexTests
import           Hunt.Index.InvertedIndexTests
import           Hunt.Index.TestHelper
-- ----------------------------------------------------------------------------

indexImplTests :: [Test]
indexImplTests =
  prefixTreeIndexTests ++
  prefixTreeIndex2DimTests ++
  rTreeIndexTests ++
  invertedIndexTests ++
  [
  -- test: insertList, deleteDocs, toList, fromList, map
  -- test: intindex, dateindex, geoindex

  -- ContextIndex implementation
    testCase "ContextIndex:            insert"        insertTestContextIx
  , testCase "ContextIndex:            insertContext" insertTestContext
  -- helper functions
  , testCase "TextIndex:               addWords"      addWordsTest
  , testCase "Occurrences:             merge"         occMergeTest
  ]

-- ----------------------------------------------------------------------------
-- Check InvertedIndex implementation

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
