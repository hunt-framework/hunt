{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Hunt.IndexTests
(contextTypeTests)
where

--import qualified Data.Map                       as M

import           Test.Framework

import qualified Hunt.Index.Default             as Default
import qualified Hunt.Index.IndexValueTests     as Value

-- ----------------------------------------------------------------------------

contextTypeTests :: [Test]
contextTypeTests =
  Default.tests ++
  Value.tests ++
  [
  -- test: insertList, deleteDocs, toList, fromList, map
  -- test: intindex, dateindex, geoindex

  -- helper functions
--    testCase "TextIndex:               addWords"      addWordsTest
--    testCase "Occurrences:             merge"         occMergeTest
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
{--
occMergeTest :: Assertion
occMergeTest = True @?= (merge occ1 occ2 == occ3)
  where
  occ1 = occOne
  occ2 = occTwo
  occ3 = insert (docIdOne) 2 $ occOne
--}
-- ----------------------------------------------------------------------------
-- test helper
-- ----------------------------------------------------------------------------

--wordList :: WordList
--wordList = M.fromList $ [("word", [1,5,10])]

--wrds :: Word -> Words
--wrds w = M.fromList $ [(w, wordList)]
