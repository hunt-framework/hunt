{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import qualified Control.Monad.Parallel                          as Par
import           Control.Monad                                   (foldM)
import           TestHelper
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Monadic                         (PropertyM,
                                                                  monadicIO,
                                                                  monitor,
                                                                  pick, run)

import qualified Data.Set                                        as S
import           Data.Text                                       (Text)

import           GHC.AssertNF
import           GHC.HeapView
import qualified System.Mem

import           Hunt.Common
import qualified Hunt.Common.Positions                       as Pos
import qualified Hunt.Common.Occurrences                     as Occ
import qualified Hunt.Common.DocIdMap                        as DM
import qualified Hunt.Common.DocIdSet                        as IS
import qualified Hunt.Common.DocDesc                         as DD

import qualified Data.Map.Strict                             as M

import qualified Hunt.Index                                  as Ix
import           Hunt.ContextIndex
import qualified Hunt.Index.IndexImpl                        as Impl
import qualified Hunt.Index.InvertedIndex                    as InvIx
import qualified Hunt.Index.PrefixTreeIndex                  as PIx
import qualified Hunt.Index.PrefixTreeIndex2Dim              as PIx2D
import qualified Hunt.Index.Proxy.KeyIndex                   as KeyProxy

import qualified Hunt.DocTable                               as Dt
import qualified Hunt.DocTable.HashedDocTable                as HDt

-- ----------------------------------------------------------------------------

main :: IO ()
main = defaultMain
  -- strictness property for data-structures used in index and
  -- document table
  [ testProperty "prop_strictness_occurrences"               prop_occs
  , testProperty "prop_strictness_document"                  prop_doc

  , testProperty "prop_strictness_docdesc empty"             prop_dd_empty
  , testProperty "prop_strictness_docdesc fromList"          prop_dd_empty
  , testProperty "prop_strictness_docdesc insert"            prop_dd_insert
  , testProperty "prop_strictness_docdesc union"             prop_dd_union

  -- strictness property for index implementations by function
  -- insert / insertList
  , testProperty "prop_strictness insert prefixtreeindex"    prop_ptix
  , testProperty "prop_strictness insert prefixtreeindex2D"  prop_ptix2d
  , testProperty "prop_strictness insert textindex"          prop_invix1
  , testProperty "prop_strictness insert numericindex"       prop_invix2
  , testProperty "prop_strictness insert dateindex"          prop_invix3
  , testProperty "prop_strictness insert geoindex"           prop_invix4
  , testProperty "prop_strictness insert proxy"              prop_proxy
  -- delete / deleteDocs
  , testProperty "prop_strictness delete prefixtreeindex"    prop_ptix_del
  , testProperty "prop_strictness delete prefixtreeindex2D"  prop_ptix2d_del
  , testProperty "prop_strictness delete textindex"          prop_invix1_del
  , testProperty "prop_strictness delete numericindex"       prop_invix2_del
  , testProperty "prop_strictness delete dateindex"          prop_invix3_del
  , testProperty "prop_strictness delete geoindex"           prop_invix4_del
  , testProperty "prop_strictness delete proxy"              prop_proxy_del
  -- map
  , testProperty "prop_strictness map prefixtreeindex"       prop_ptix_map
  , testProperty "prop_strictness map prefixtreeindex2D"     prop_ptix2d_map
  , testProperty "prop_strictness map textindex"             prop_invix1_map
  , testProperty "prop_strictness map numericindex"          prop_invix2_map
  , testProperty "prop_strictness map dateindex"             prop_invix3_map
  , testProperty "prop_strictness map geoindex"              prop_invix4_map
  , testProperty "prop_strictness map proxy"                 prop_proxy_map
  -- mapMaybe
  , testProperty "prop_strictness mapMaybe prefixtreeindex"  prop_ptix_map2
  , testProperty "prop_strictness mapMaybe prefixtreeinde2d" prop_ptix2d_map2
  , testProperty "prop_strictness mapMaybe textindex"        prop_invix1_map2
  , testProperty "prop_strictness mapMaybe numericindex"     prop_invix2_map2
  , testProperty "prop_strictness mapMaybe dateindex"        prop_invix3_map2
  , testProperty "prop_strictness mapMaybe geoindex"         prop_invix4_map2
  , testProperty "prop_strictness mapMaybe proxy"            prop_proxy_map2
  -- unionWith
  , testProperty "prop_strictness unionWith prefixtreeindex" prop_ptix_union
  , testProperty "prop_strictness unionWith prefixtreeind2d" prop_ptix2d_union
  , testProperty "prop_strictness unionWith textindex"       prop_invix1_union
  , testProperty "prop_strictness unionWith numericindex"    prop_invix2_union
  , testProperty "prop_strictness unionWith dateindex"       prop_invix3_union
  , testProperty "prop_strictness unionWith geoindex"        prop_invix4_union
  , testProperty "prop_strictness unionWith proxy"           prop_proxy_union

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

  , testProperty "prop_strictness_insertList1"               prop_cx_insertlist
  , testProperty "prop_strictness_insertList2"               prop_cx_insertlist2
  , testProperty "prop_strictness_insertList3"               prop_cx_insertlist3



  -- TODO:
  -- map
  -- filter
  -- mapKeys
  ]

-- ----------------------------------------------------------------------------
-- test data structures
-- ----------------------------------------------------------------------------

prop_occs :: Property
prop_occs = monadicIO $ do
  x <- pick arbitrary :: PropertyM IO Occurrences
  assertNF' $! x

prop_doc :: Property
prop_doc = monadicIO $ do
  x <- pick arbitrary :: PropertyM IO Document
  assertNF' $! x

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

-- ----------------------------------------------------------------------------
-- document table implementation
-- ----------------------------------------------------------------------------
instance Par.MonadParallel (PropertyM IO) where

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
  assertNF' dts
  -- create input list to work with
  let dt = if length dts > 0 then (head dts) else Dt.empty
  input <- mapM (\dt' -> return (dt',[])) $ drop 1 dts
  -- union doctables with insertLists reduce function and check result for strictness
  out   <- unionDocTables input dt
  assertNF' out

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

-- ----------------------------------------------------------------------------
-- index implementations: insert function
-- ----------------------------------------------------------------------------

prop_ptix :: Property
prop_ptix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.DmPrefixTree Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert Occ.merge "key" val Ix.empty

prop_ptix2d :: Property
prop_ptix2d
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.DmPrefixTree Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert Occ.merge "11" val Ix.empty

prop_invix1 :: Property
prop_invix1
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert Occ.merge "key" val Ix.empty

prop_invix2 :: Property
prop_invix2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexInt Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert Occ.merge "1" val Ix.empty

prop_invix3 :: Property
prop_invix3
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexDate Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert Occ.merge "2013-01-01" val Ix.empty

prop_invix4 :: Property
prop_invix4
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexPosition Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert Occ.merge "1-1" val Ix.empty

prop_proxy :: Property
prop_proxy
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree) Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert Occ.merge "key" val Ix.empty

-- ----------------------------------------------------------------------------
-- index implementations: delete function
-- ----------------------------------------------------------------------------

prop_ptix_del :: Property
prop_ptix_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.DmPrefixTree Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "key"


prop_ptix2d_del :: Property
prop_ptix2d_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.DmPrefixTree Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "11"

prop_invix1_del :: Property
prop_invix1_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "key"

prop_invix2_del :: Property
prop_invix2_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexInt Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "1"

prop_invix3_del :: Property
prop_invix3_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexDate Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "2013-01-01"

prop_invix4_del :: Property
prop_invix4_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexPosition Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "1-1"

prop_proxy_del :: Property
prop_proxy_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree) Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "key"

--insert_and_delete :: forall v (m :: * -> *) (i :: * -> *) v1.
--                     (Ix.ICon i v1, Monad m, Ix.Index i, Ix.IVal i v1 ~ DocIdMap v) =>
--                     Ix.IKey i v1 -> DocIdMap v -> m (i v1)
insert_and_delete :: forall (m :: * -> *) (i :: * -> *) v.
                     (Ix.ICon i v, Monad m, Ix.Index i,
                      Ix.IVal i v ~ DocIdMap Positions) =>
                      Ix.IKey i v -> DocIdMap Positions -> m (i v)
insert_and_delete key v
  = return $ Ix.delete docId
           $ Ix.insert Occ.merge key v
           $ Ix.empty
    where
    docId = case DM.toList v of
              ((did,_):_) -> did
              _           -> mkDocId (0::Int)

-- ----------------------------------------------------------------------------
-- index implementations: map function
-- ----------------------------------------------------------------------------

prop_ptix_map :: Property
prop_ptix_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.DmPrefixTree Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map "key"

prop_ptix2d_map :: Property
prop_ptix2d_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.DmPrefixTree Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map "11"

prop_invix1_map :: Property
prop_invix1_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map "key"

prop_invix2_map :: Property
prop_invix2_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexInt Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map "1"

prop_invix3_map :: Property
prop_invix3_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexDate Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map "2013-01-01"

prop_invix4_map :: Property
prop_invix4_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexPosition Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map "1-1"

prop_proxy_map :: Property
prop_proxy_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree) Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map "key"

insert_and_map :: forall (m :: * -> *) (i :: * -> *) v.
                  (Ix.ICon i v, Monad m, Ix.Index i,
                   Ix.IVal i v ~ DocIdMap Positions) =>
                  Ix.IKey i v -> DocIdMap Positions -> m (i v)
insert_and_map key v
  = return $ Ix.map (DM.insert (mkDocId (1 :: Int)) (Pos.singleton 1))
           $ Ix.insert Occ.merge key v Ix.empty

-- ----------------------------------------------------------------------------
-- index implementations: mapMaybe function
-- ----------------------------------------------------------------------------

prop_ptix_map2 :: Property
prop_ptix_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.DmPrefixTree Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2 "key"

prop_ptix2d_map2 :: Property
prop_ptix2d_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.DmPrefixTree Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2 "11"

prop_invix1_map2 :: Property
prop_invix1_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2 "key"

prop_invix2_map2 :: Property
prop_invix2_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexInt Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2 "1"

prop_invix3_map2 :: Property
prop_invix3_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexDate Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2 "2013-01-01"

prop_invix4_map2 :: Property
prop_invix4_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexPosition Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2 "1-1"

prop_proxy_map2 :: Property
prop_proxy_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree) Positions)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2 "key"

insert_and_map2 :: forall (m :: * -> *) (i :: * -> *) v.
                   (Ix.ICon i v, Monad m, Ix.Index i,
                    Ix.IVal i v ~ DocIdMap Positions) =>
                   Ix.IKey i v -> DocIdMap Positions -> m (i v)
insert_and_map2 key v
  = return $ Ix.mapMaybe (Just . DM.insert (mkDocId (1 :: Int)) (Pos.singleton 1))
           $ Ix.insert Occ.merge key v Ix.empty

-- ----------------------------------------------------------------------------
-- index implementations: unionWith function
-- ----------------------------------------------------------------------------

prop_ptix_union :: Property
prop_ptix_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.DmPrefixTree Positions)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary
    val2 <- pick arbitrary
    insert_and_union "key" val1 val2

prop_ptix2d_union :: Property
prop_ptix2d_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.DmPrefixTree Positions)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary
    val2 <- pick arbitrary
    insert_and_union "11" val1 val2

prop_invix1_union :: Property
prop_invix1_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex Positions)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary
    val2 <- pick arbitrary
    insert_and_union "key" val1 val2

prop_invix2_union :: Property
prop_invix2_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexInt Positions)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary
    val2 <- pick arbitrary
    insert_and_union "1" val1 val2

prop_invix3_union :: Property
prop_invix3_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexDate Positions)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary
    val2 <- pick arbitrary
    insert_and_union "2013-01-01" val1 val2

prop_invix4_union :: Property
prop_invix4_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexPosition Positions)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary
    val2 <- pick arbitrary
    insert_and_union "1-1" val1 val2

prop_proxy_union :: Property
prop_proxy_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree) Positions)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary
    val2 <- pick arbitrary
    insert_and_union "key" val1 val2

insert_and_union :: forall (m :: * -> *) (i :: * -> *) v.
                    (Ix.ICon i v, Monad m, Ix.Index i,
                     Ix.IVal i v ~ DocIdMap Positions) =>
                     Ix.IKey i v -> DocIdMap Positions -> DocIdMap Positions -> m (i v)
insert_and_union key v1 v2
  = return $ Ix.unionWith (DM.union)
             (Ix.insert Occ.merge key v1 Ix.empty)
             (Ix.insert Occ.merge key v2 Ix.empty)


-- ----------------------------------------------------------------------------
-- test property contextindex: cannot be tested right now because of
-- existential quanticication
-- ----------------------------------------------------------------------------
{--
prop_contextix_empty :: Property
prop_contextix_empty
  = monadicIO $ do
    assertNF' CIx.empty


prop_contextix_emptyix :: Property
prop_contextix_emptyix
   = monadicIO $ do
    val     <- pick arbitrary :: PropertyM IO Occurrences
    let ix  = Ix.empty :: (InvIx.InvertedIndexPosition Occurrences)
    let ix2 = Ix.empty :: (InvIx.InvertedIndex Occurrences)
    let cix = CIx.insertContext "text" (mkIndex ix2)
            $ CIx.insertContext "geo" (mkIndex ix)
            $ CIx.empty
    assertNF' cix



prop_contextix :: Property
prop_contextix
  = monadicIO $ do
    val     <- pick arbitrary :: PropertyM IO Occurrences
    let ix  = Ix.empty :: (InvIx.InvertedIndexPosition Occurrences)
    let ix2 = Ix.empty :: (InvIx.InvertedIndex Occurrences)
    let cix = CIx.insertWithCx "text" "word" val
            $ CIx.insertContext "text" (mkIndex ix2)
            $ CIx.insertContext "geo" (mkIndex ix)
            $ CIx.empty
    assertNF' cix


prop_contextix2 :: Property
prop_contextix2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexPosition Occurrences)
    ix2 <- pickIx2 :: PropertyM IO (InvIx.InvertedIndex Occurrences)
    let cix = CIx.insertContext "text" (mkIndex ix2)
            $ CIx.insertContext "geo" (mkIndex ix)
            $ CIx.empty
    assertNF' cix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert Occ.merge "1-1" val Ix.empty
  pickIx2 = pick arbitrary >>= \val -> return $ Ix.insert Occ.merge "1-1" val Ix.empty
--}

-- ----------------------------------------------------------------------------
-- test property indeximpl: cannot be tested right now, because of
-- existential quantification
-- ----------------------------------------------------------------------------

{--
prop_impl_full :: Property
prop_impl_full
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndexPosition Occurrences)
    assertNF' $ mkIndex ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert Occ.merge "1-1" val Ix.empty

prop_impl_empty :: Property
prop_impl_empty
  = monadicIO $ do
    let ix = Ix.empty  :: (InvIx.InvertedIndexPosition Occurrences)
    assertNF' $ mkIndex ix
--}

-- ------------------------------------------------------------

heapGraph :: Int -> a -> IO String
heapGraph d x = do
  let box = asBox x
  graph <- buildHeapGraph d () box
  return $ ppHeapGraph graph

isNFWithGraph :: Int -> a -> IO (Bool, String)
isNFWithGraph d x = do
  b <- isNF $! x
  -- XXX: does gc need a delay?
  System.Mem.performGC
  g <- heapGraph d x
  return (b,g)

-- depth is a constant
assertNF' :: a -> PropertyM IO ()
assertNF' = assertNF'' 5

assertNF'' :: Int -> a -> PropertyM IO ()
assertNF'' d x = do
  (b,g) <- run $ isNFWithGraph d x
  monitor $ const $ counterexample  g b

-- ----------------------------------------------------------------------------
