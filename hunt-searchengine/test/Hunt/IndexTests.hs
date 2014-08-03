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
import           Hunt.Common.Occurrences

--import           Hunt.ContextIndex              (addWordsM)
--import qualified Hunt.ContextIndex              as ConIx

import           Hunt.Index.PrefixTreeIndexTests
import           Hunt.Index.PrefixTreeIndex2DimTests
import           Hunt.Index.RTreeIndexTests
import           Hunt.Index.InvertedIndexTests
import           Hunt.Index.ContextIndexTests
import           Hunt.Index.TestHelper
--import           Hunt.TestHelper

-- ----------------------------------------------------------------------------

indexImplTests :: [Test]
indexImplTests =
  prefixTreeIndexTests ++
  prefixTreeIndex2DimTests ++
  rTreeIndexTests ++
  invertedIndexTests ++
  contextIndexTests ++
  [
  -- test: insertList, deleteDocs, toList, fromList, map
  -- test: intindex, dateindex, geoindex

  -- helper functions
--    testCase "TextIndex:               addWords"      addWordsTest
    testCase "Occurrences:             merge"         occMergeTest
  ]

-- ----------------------------------------------------------------------------
-- check helper functions
-- ----------------------------------------------------------------------------

{--addWordsTest :: Assertion
addWordsTest = do
  resIx   <- addWordsM (wrds "default") docIdOne emptyIndex
  resList <- ConIx.searchWithCx PrefixNoCase "default" "word" $ resIx
  True @?= length resList == 1
  where
  (ConIx.ContextIndex emptyIndex _ _) = insertCx "default"
--}

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
