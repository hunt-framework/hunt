{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Map                                    as M
import           Data.Monoid

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

import           Hunt.Common.BasicTypes
import qualified Hunt.Common.DocIdMap                        as DM
import           Hunt.Common.DocIdMap                        (DocIdMap)
import           Hunt.Common.Positions                       (Positions)
import           Hunt.Common.Occurrences
import           Hunt.Common.Document

import           Hunt.ContextIndex                           (addWordsM)
import qualified Hunt.ContextIndex                           as ConIx
import qualified Hunt.Index                                  as Ix
import           Hunt.Index.IndexImpl
import qualified Hunt.Index.InvertedIndex                    as InvIx
import qualified Hunt.Index.PrefixTreeIndex                  as PIx

-- ----------------------------------------------------------------------------

main :: IO ()
main = defaultMainWithOpts
  [ testCase "DmPrefixTree:            insert"        insertTestPIx
  , testCase "InvertedIndex:           insert"        insertTestInvIx
  , testCase "InvertedIndex:           deleteDocs"    deleteDocsTestInvIx
  , testCase "InvertedIndex:           merge"         mergeTestInvIx
  , testCase "ContextMap Inverted:     insert"        insertTestContextIx
  , testCase "ContextMap Inverted:     insertContext" insertTestContext
  , testCase "TextIndex:               addWords"      addWordsTest
  ] mempty

-- check if element is inserted in insert operation

-- | general check function
insertTest :: (Monad m, Ix.Index i, Eq (Ix.IVal i v), (Ix.ICon i v)) =>
              i v -> Ix.IKey i v -> Ix.IVal i v -> m Bool
insertTest emptyIndex k v = do
  ix       <- Ix.insertM k v emptyIndex
  [(_,nv)] <- Ix.searchM PrefixNoCase k ix
  return $ v == nv


-- v1 and v2 need the have the same id
mergeTest :: (Monad m, Ix.ICon i v, Ix.Index i
             , Ix.IVal i v ~ DocIdMap Positions) =>
             i v -> Ix.IKey i v -> Occurrences -> Occurrences -> m Bool
mergeTest emptyIndex k v1 v2 = do
  mergeIx      <- Ix.insertM k v1 emptyIndex >>= \ix -> Ix.insertM k v2 ix
  res@[(_,nv)] <- Ix.searchM PrefixNoCase k mergeIx
  return $ length res == 1 && merge v1 v2 == nv


-- | check DmPrefixTree
insertTestPIx :: Assertion
insertTestPIx = do
  result <- insertTest (Ix.empty::(PIx.DmPrefixTree Positions)) "test" (singleton 1 1)
  True @?= result


-- | check InvertedIndex
insertTestInvIx :: Assertion
insertTestInvIx = do
  result <- insertTest
            (Ix.empty::(InvIx.InvertedIndex Occurrences)) -- the Occurrences type is a dummy in this case
            "test" (singleton 1 1)
  True @?= result

-- | check deleteDocs with InvertedIndex
deleteDocsTestInvIx :: Assertion
deleteDocsTestInvIx = do
  let occ = (singleton 1 1)
  ix <- Ix.insertM "test" occ (Ix.empty::(InvIx.InvertedIndex Occurrences)) -- the Occurrences type is a dummy in this case
  [(_,nv)] <- Ix.searchM PrefixNoCase "test" ix
  nv @?= occ
  let delNot = DM.toDocIdSet [2]
  ix' <- Ix.deleteDocsM delNot ix
  [(_,nv')] <- Ix.searchM PrefixNoCase "test" ix'
  nv' @?= occ
  let delReal = DM.toDocIdSet [1]
  ix'' <- Ix.deleteDocsM delReal ix'
  result <- Ix.searchM PrefixNoCase "test" ix''
  result @?= []

-- | test merging of occurrences in InvertedIndex
mergeTestInvIx :: Assertion
mergeTestInvIx = do
  result <- mergeTest
            (Ix.empty::(InvIx.InvertedIndex Occurrences)) -- the Occurrences type is a dummy in this case
            "test" (singleton 1 1) (singleton 1 2)
  True @?= result

-- | check ContextMap
insertTestContextIx :: Assertion
insertTestContextIx
  = do
    let cix = ConIx.insertContext "context" (mkIndex impl) emptyIndex
    cix2 <- ConIx.insertWithCx "context" "word" newElem cix
    [(_, insertedElem)] <- ConIx.searchWithCx PrefixNoCase "context" "word" cix2
    True @?= newElem == insertedElem
  where
  newElem = singleton 1 1
  emptyIndex :: ConIx.ContextMap Occurrences
  emptyIndex = ConIx.empty
  impl       :: InvIx.InvertedIndex Occurrences
  impl       = Ix.empty

insertTestContext :: Assertion
insertTestContext = "test" @?= insertedContext
  where
  [insertedContext] = ConIx.contexts' ix
  ix :: ConIx.ContextMap Occurrences
  ix = ConIx.insertContext "test" (mkIndex (Ix.empty :: InvIx.InvertedIndex Occurrences)) ConIx.empty

-- ----------------------------------------------------------------------------
-- check helper functions
-- ----------------------------------------------------------------------------

addWordsTest :: Assertion
addWordsTest = do
  resIx   <- addWordsM (wrds "default") 1 emptyIndex
  resList <- ConIx.searchWithCx PrefixNoCase "default" "word" $ resIx
  True @?= length resList == 1
  where
  emptyIndex :: ConIx.ContextMap Occurrences
  emptyIndex =  ConIx.insertContext "default" (mkIndex impl) ConIx.empty
  impl       :: InvIx.InvertedIndex Occurrences
  impl       = Ix.empty

-- ----------------------------------------------------------------------------
-- helper
-- ----------------------------------------------------------------------------

wordList :: WordList
wordList = M.fromList $ [("word", [1,5,10])]

wrds :: Word -> Words
wrds w = M.fromList $ [(w, wordList)]

doc :: Document
doc = Document "id::1" (M.fromList [("name", "Chris"), ("alter", "30")]) 1.0
