	{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Data.Map                             as M
import           Data.Monoid

import           Test.Framework
import           Test.Framework.Providers.HUnit
--import          Test.Framework.Providers.QuickCheck2
import           Test.HUnit
--import          Test.QuickCheck

import           Hunt.Common
import           Hunt.Common.Occurrences          (merge, singleton)
import           Hunt.Common.Occurrences.Compression.Simple9

import qualified Hunt.Index.Index                 as Ix
import qualified Hunt.Index.InvertedIndex         as InvIx
import qualified Hunt.Index.PrefixTreeIndex       as PIx
import qualified Hunt.Index.Proxy.CompressedIndex as CPIx
import qualified Hunt.Index.Proxy.ContextIndex    as ConIx
import           Hunt.Index.IndexImpl
import           Hunt.IndexHandler                (addWords)
-- ----------------------------------------------------------------------------

main :: IO ()
main = defaultMainWithOpts
  [ testCase "DmPrefixTree:            insert"        insertTestPIx
  , testCase "InvertedIndex:           insert"        insertTestInvIx
  , testCase "InvertedIndex:           merge"         mergeTestInvIx
  , testCase "ComprOccPrefixTree:      insert"        insertTestCPIx
  , testCase "ContextIndex Inverted:   insert"        insertTestContextIx
  , testCase "ContextIndex Inverted:   insertContext" insertTestContext
  , testCase "TextIndex:               addWords"      addWordsTest
  ] mempty

-- check if element is inserted in insert operation

-- | general check function
insertTest :: (Monad m, Ix.Index i, Eq (Ix.IVal i v), (Ix.ICon i v)) =>
              i v -> Ix.IKey i v -> Ix.IVal i v -> m Bool
insertTest emptyIndex k v = do
  ix       <- Ix.insert k v emptyIndex
  [(_,nv)] <- Ix.search PrefixNoCase k ix
  return $ v == nv


-- v1 and v2 need the have the same id
mergeTest :: (Monad m, Ix.ICon i v, Ix.Index i
             , Ix.IVal i v ~ DocIdMap Positions) =>
             i v -> Ix.IKey i v -> Occurrences -> Occurrences -> m Bool
mergeTest emptyIndex k v1 v2 = do
  mergeIx      <- Ix.insert k v1 emptyIndex >>= \ix -> Ix.insert k v2 ix
  res@[(_,nv)] <- Ix.search PrefixNoCase k mergeIx
  return $ length res == 1 && merge v1 v2 == nv


-- | check DmPrefixTree
insertTestPIx :: Assertion
insertTestPIx = do
  result <- insertTest (Ix.empty::(PIx.DmPrefixTree Positions)) "test" (singleton 1 1)
  True @?= result

-- | check ComprOccPrefixTree
insertTestCPIx :: Assertion
insertTestCPIx = do
  result <- insertTest
            (Ix.empty::((CPIx.ComprOccIndex PIx.DmPrefixTree CompressedPositions) Positions))
            "test" (singleton 1 1)
  True @?= result


-- | check InvertedIndex
insertTestInvIx :: Assertion
insertTestInvIx = do
  result <- insertTest
            (Ix.empty::(InvIx.InvertedIndex Occurrences)) -- the Occurrences type is a dummy in this case
            "test" (singleton 1 1)
  True @?= result

-- | test merging of occurrences in InvertedIndex
mergeTestInvIx :: Assertion
mergeTestInvIx = do
  result <- mergeTest
            (Ix.empty::(InvIx.InvertedIndex Occurrences)) -- the Occurrences type is a dummy in this case
            "test" (singleton 1 1) (singleton 1 2)
  True @?= result

-- | check ContextIndex
insertTestContextIx :: Assertion
insertTestContextIx
  = do
    let cix = ConIx.insertContext "context" (mkIndex impl) emptyIndex
    cix2 <- ConIx.insertWithCx "context" "word" newElem cix
    [(_, insertedElem)] <- ConIx.searchWithCx PrefixNoCase "context" "word" cix2
    True @?= newElem == insertedElem
  where
  newElem = singleton 1 1
  emptyIndex :: ConIx.ContextIndex Occurrences
  emptyIndex = ConIx.empty
  impl       :: InvIx.InvertedIndex Occurrences
  impl       = Ix.empty

insertTestContext :: Assertion
insertTestContext = "test" @?= insertedContext
  where
  [insertedContext] = ConIx.contexts ix
  ix :: ConIx.ContextIndex Occurrences
  ix = ConIx.insertContext "test" (mkIndex (Ix.empty :: InvIx.InvertedIndex Occurrences)) ConIx.empty

-- ----------------------------------------------------------------------------
-- check helper functions
-- ----------------------------------------------------------------------------

addWordsTest :: Assertion
addWordsTest = do
  resIx   <- addWords (wrds "default") 1 emptyIndex
  resList <- ConIx.searchWithCx PrefixNoCase "default" "word" $ resIx
  True @?= length resList == 1
  where
  emptyIndex :: ConIx.ContextIndex Occurrences
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
doc = Document "id::1" (M.fromList [("name", "Chris"), ("alter", "30")])
