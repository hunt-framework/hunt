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

import           Holumbus.Common
import           Holumbus.Common.Occurrences          (merge, singleton)
import           Holumbus.Common.Occurrences.Compression.Simple9

import qualified Holumbus.Index.Index                 as Ix
import qualified Holumbus.Index.InvertedIndex         as InvIx
import qualified Holumbus.Index.PrefixTreeIndex       as PIx
import qualified Holumbus.Index.Proxy.CompressedIndex as CPIx
import qualified Holumbus.Index.Proxy.ContextIndex    as ConIx
import           Holumbus.Index.TextIndex
import           Holumbus.Index.IndexImpl

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
insertTest :: (Ix.ISearchOp i v ~ TextSearchOp, Ix.Index i, Eq (Ix.IVal i v), (Ix.ICon i v)) =>
              i v -> Ix.IKey i v -> Ix.IVal i v -> Bool
insertTest emptyIndex k v = v == nv
  where
  [(_,nv)] = Ix.search PrefixNoCase k $ Ix.insert k v emptyIndex


-- v1 and v2 need the have the same id
mergeTest :: (Ix.ICon i v, Ix.Index i, Ix.ISearchOp i v ~ TextSearchOp
             , Ix.IVal i v ~ DocIdMap Positions) =>
             i v -> Ix.IKey i v -> Occurrences -> Occurrences -> Bool
mergeTest emptyIndex k v1 v2 = length res == 1 && merge v1 v2 == nv
  where
  res@[(_,nv)] = Ix.search PrefixNoCase k $ Ix.insert k v1 . Ix.insert k v2 $ emptyIndex


-- | check DmPrefixTree
insertTestPIx :: Assertion
insertTestPIx
  = True @?= insertTest
    (Ix.empty::(PIx.DmPrefixTree Positions))
    "test"
    (singleton 1 1)

-- | check ComprOccPrefixTree
insertTestCPIx :: Assertion
insertTestCPIx
  = True @?= insertTest
    (Ix.empty::((CPIx.ComprOccIndex PIx.DmPrefixTree CompressedPositions) Positions))
    "test"
    (singleton 1 1)


-- | check InvertedIndex
insertTestInvIx :: Assertion
insertTestInvIx
  = True @?= insertTest
    (Ix.empty::(InvIx.InvertedIndex Occurrences)) -- the Occurrences type is a dummy in this case
    "test"
    (singleton 1 1)

-- | test merging of occurrences in InvertedIndex
mergeTestInvIx :: Assertion
mergeTestInvIx
  = True @?= mergeTest
    (Ix.empty::(InvIx.InvertedIndex Occurrences)) -- the Occurrences type is a dummy in this case
    "test"
    (singleton 1 1)
    (singleton 1 2)

-- | check ContextIndex
insertTestContextIx :: Assertion
insertTestContextIx
  = do
    True @?= newElem == insertedElem
  where
  newElem = singleton 1 1
  [(_, insertedElem)] = ConIx.searchWithCx PrefixNoCase "context" "word"
                            $ ConIx.insertWithCx "context" "word" newElem emptyIndex
  emptyIndex :: ConIx.ContextIndex Occurrences
  emptyIndex = ConIx.empty

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
addWordsTest = True @?= length resList == 1
  where
  resList = ConIx.searchWithCx PrefixNoCase "default" "word" $ resIx
  resIx = addWords (wrds "default") 1 emptyIndex
  emptyIndex :: ConIx.ContextIndex Occurrences
  emptyIndex =  ConIx.empty

-- ----------------------------------------------------------------------------
-- helper
-- ----------------------------------------------------------------------------

wordList :: WordList
wordList = M.fromList $ [("word", [1,5,10])]

wrds :: Word -> Words
wrds w = M.fromList $ [(w, wordList)]

doc :: Document
doc = Document "id::1" (M.fromList [("name", "Chris"), ("alter", "30")])
