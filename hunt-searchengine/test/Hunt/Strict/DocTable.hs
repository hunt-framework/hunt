{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hunt.Strict.DocTable
(docTableTests)
where

import           Control.Monad                                   (foldM)
import           Hunt.TestHelper
import           Hunt.Strict.Helper
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Monadic                         (PropertyM,
                                                                  monadicIO,
                                                                  pick)

import qualified Data.Set                                        as S
import           Data.Text                                       (Text)

import           Hunt.Common
import qualified Hunt.Common.DocIdSet                        as IS
import qualified Hunt.Common.DocDesc                         as DD

import qualified Hunt.DocTable                               as Dt
import qualified Hunt.DocTable.HashedDocTable                as HDt

-- ----------------------------------------------------------------------------

docTableTests :: [Test]
docTableTests =
  -- tests for data-structures used in contexts of the document table
  [ testProperty "prop_strictness_document"                  prop_doc

  , testProperty "prop_strictness_docdesc empty"             prop_dd_empty
  , testProperty "prop_strictness_docdesc fromList"          prop_dd_fromList
  , testProperty "prop_strictness_docdesc insert"            prop_dd_insert
  , testProperty "prop_strictness_docdesc union"             prop_dd_union


  -- strictness property for document table by function
  , testProperty "prop_strictness union doctable"            prop_dt_union
  , testProperty "prop_strictness insert doctable"           prop_dt_insert
  , testProperty "prop_strictness update doctable"           prop_dt_update
  , testProperty "prop_strictness delete doctable"           prop_dt_delete
  , testProperty "prop_strictness delbyuri doctable"         prop_dt_delete2
  , testProperty "prop_strictness adjust doctable"           prop_dt_adjust
  , testProperty "prop_strictness adjuri doctable"           prop_dt_adjust2
  , testProperty "prop_strictness difference doctable"       prop_dt_difference
  , testProperty "prop_strictness diffusi doctable"          prop_dt_difference2
  -- TODO:
  -- map
  -- filter
  -- mapKeys
  ]

-- ----------------------------------------------------------------------------
-- test data structures: Document
-- ----------------------------------------------------------------------------

prop_doc :: Property
prop_doc = monadicIO $ do
  x <- pick arbitrary :: PropertyM IO Document
  assertNF' $! x

-- ----------------------------------------------------------------------------
-- test data structures: Document description (DocDesc)
-- ----------------------------------------------------------------------------

prop_dd_empty :: Property
prop_dd_empty = monadicIO $ do
  assertNF' $! DD.empty

prop_dd_fromList :: Property
prop_dd_fromList = monadicIO $ do
  v1 <- pick niceText1
  v2 <- pick niceText1
  k1 <- pick niceText1
  k2 <- pick niceText1
  assertNF' $! DD.fromList (list k1 v1 k2 v2)
  where
  list k1 v1 k2 v2 = [(k1,v1), (k2,v2)] :: [(Text,Text)]

prop_dd_insert :: Property
prop_dd_insert = monadicIO $ do
  assertNF' $! DD.insert "key" ("value"::String) DD.empty

prop_dd_union :: Property
prop_dd_union = monadicIO $ do
  x <- pick mkDescription
  y <- pick mkDescription
  assertNF' $! DD.union x y



-- ----------------------------------------------------------------------------
-- document table implementation
-- ----------------------------------------------------------------------------

prop_dt_insert :: Property
prop_dt_insert
  = monadicIO $ do
    (_,dt) <- pickIx :: PropertyM IO (DocId, HDt.Documents Document)
    assertNF' dt
  where
  pickIx = pick arbitrary >>= \doc -> Dt.insert doc Dt.empty

prop_dt_union :: Property
prop_dt_union
  = monadicIO $ do
    dt <- pick mkDocTables >>= foldM Dt.union Dt.empty
    assertNF' dt

prop_dt_update :: Property
prop_dt_update
  = monadicIO $ do
    doc1 <- pick mkDocument'
    (docid, dt1) <- Dt.insert doc1 (Dt.empty :: HDt.Documents Document)
    doc2 <- pick mkDocument'
    Dt.update docid doc2 dt1

prop_dt_delete :: Property
prop_dt_delete
  = monadicIO $ do
    dt <- pickIx :: PropertyM IO (HDt.Documents Document)
    assertNF' dt
  where
  pickIx = do
    doc1 <- pick arbitrary
    doc2 <- pick arbitrary
    (docid, dt) <- Dt.insert doc1 Dt.empty
    (_, dt')    <- Dt.insert doc2 dt
    Dt.delete docid dt'

prop_dt_delete2 :: Property
prop_dt_delete2
  = monadicIO $ do
    dt <- pickIx :: PropertyM IO (HDt.Documents Document)
    assertNF' dt
  where
  pickIx = do
    doc1@(Document u _ _ _) <- pick arbitrary
    doc2 <- pick arbitrary
    (_, dt) <- Dt.insert doc1 Dt.empty
    (_, dt')    <- Dt.insert doc2 dt
    Dt.deleteByURI u dt'

prop_dt_adjust :: Property
prop_dt_adjust
  = monadicIO $ do
    dt <- pickIx :: PropertyM IO (HDt.Documents Document)
    assertNF' dt
  where
  pickIx = do
    doc1 <- pick arbitrary
    doc2 <- pick arbitrary
    (docid, dt) <- Dt.insert doc1 Dt.empty
    Dt.adjust (\_ -> return doc2) docid dt


prop_dt_adjust2 :: Property
prop_dt_adjust2
  = monadicIO $ do
    dt <- pickIx :: PropertyM IO (HDt.Documents Document)
    assertNF' dt
  where
  pickIx = do
    doc1@(Document u _ _ _) <- pick arbitrary
    doc2 <- pick arbitrary
    (_, dt) <- Dt.insert doc1 Dt.empty
    Dt.adjustByURI (\_ -> return doc2) u dt


prop_dt_difference :: Property
prop_dt_difference
  = monadicIO $ do
    dt <- pickIx :: PropertyM IO (HDt.Documents Document)
    assertNF' dt
  where
  pickIx = do
    doc1 <- pick arbitrary
    doc2 <- pick arbitrary
    (docid, dt) <- Dt.insert doc1 Dt.empty
    (_, dt')    <- Dt.insert doc2 dt
    Dt.difference (IS.singleton docid) dt'

prop_dt_difference2 :: Property
prop_dt_difference2
  = monadicIO $ do
    dt <- pickIx :: PropertyM IO (HDt.Documents Document)
    assertNF' dt
  where
  pickIx = do
    doc1@(Document u _ _ _) <- pick arbitrary
    doc2 <- pick arbitrary
    (_, dt) <- Dt.insert doc1 Dt.empty
    (_, dt')    <- Dt.insert doc2 dt
    Dt.differenceByURI (S.singleton u) dt'
