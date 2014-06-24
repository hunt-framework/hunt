{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hunt.Strict.ContextIndex
(contextIndexTests)
where

import qualified Control.Monad.Parallel                          as Par
import           Hunt.TestHelper
import           Hunt.Strict.Helper

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Monadic                         (PropertyM,
                                                                  monadicIO,
                                                                  pick)

import           Hunt.Common
import qualified Hunt.Common.Occurrences                     as Occ

import qualified Data.Map.Strict                             as M

import qualified Hunt.Index                                  as Ix
import           Hunt.ContextIndex
import qualified Hunt.Index.IndexImpl                        as Impl
import qualified Hunt.Index.InvertedIndex                    as InvIx

import qualified Hunt.DocTable                               as Dt
import qualified Hunt.DocTable.HashedDocTable                as HDt

-- ----------------------------------------------------------------------------

contextIndexTests :: [Test]
contextIndexTests =
  [ testProperty "prop_strictness_insertList1"               prop_cx_insertlist
  , testProperty "prop_strictness_insertList2"               prop_cx_insertlist2
  , testProperty "prop_strictness_insertList3"               prop_cx_insertlist3
  ]

-- ----------------------------------------------------------------------------
-- context index implementation
-- ----------------------------------------------------------------------------

prop_cx_insertlist3 :: Property
prop_cx_insertlist3 = monadicIO $ do
  -- generate list of distinct documents (in terms of uri)
  documents <- pick mkDocuments
  -- genearte mock ContextIndex to work with.
  -- Use some of the documents to be initially stored in the
  -- document table
  cxIx <- pickContextIx $ take 10 documents
 -- generate mock document-word pairs to insert.
 -- use rest of documents for this list
  insertData <- pick $ mkInsertList $ drop 10 documents
  -- check resulting document table for strictness property
  (ContextIndex _ dt' _) <- insertList insertData cxIx
  assertNF' dt'
  where
    pickIx = pick arbitrary >>= \val -> return $ Ix.insert Occ.merge "key" val Ix.empty
    pickContextIx docs = do
      ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex Occurrences)
      let cxmap = mkContextMap $ M.fromList [("context", Impl.mkIndex ix)]
      dt <- pick $ mkDocTable docs
      return $ ContextIndex cxmap dt M.empty

prop_cx_insertlist ::Property
prop_cx_insertlist = monadicIO $ do
  (table, idsAndWords) <- pickRes :: PropertyM IO (HDt.Documents Document, [(DocId, Words)])
  assertNF' table
  assertNF' idsAndWords
  where
    pickRes = pick mkInsertList' >>= createDocTableFromPartition

prop_cx_insertlist2 ::Property
prop_cx_insertlist2 = monadicIO $ do
  -- generate list of doctables and chekc if they are strict
  dts <- pick mkDocTables
  -- create input list to work with
  let dt = if length dts > 0 then (head dts) else Dt.empty
  input <- mapM (\dt' -> return (dt',[])) $ drop 1 dts
  -- union doctables with insertLists reduce function and check result for strictness
  (outDt,_) <- unionDocTables input dt
  assertNF' outDt
