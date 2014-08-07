{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ExistentialQuantification #-}

module Hunt.Strict.Index
(indexTests)
where

import           Hunt.TestHelper                                 ()
import           Hunt.Strict.Helper
import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck
import           Test.QuickCheck.Monadic                         (PropertyM,
                                                                  monadicIO,
                                                                  pick)

import           Data.Text                                       (Text)

import           Hunt.Common
import qualified Hunt.Common.Positions                       as Pos
import qualified Hunt.Common.DocIdMap                        as DM
import qualified Hunt.Common.DocIdSet                        as DS

import           Hunt.Common.IntermediateValue

import qualified Hunt.Index                                  as Ix
import qualified Hunt.Index.InvertedIndex                    as InvIx
import qualified Hunt.Index.PrefixTreeIndex                  as PIx
import qualified Hunt.Index.PrefixTreeIndex2Dim              as PIx2D
import qualified Hunt.Index.Proxy.KeyIndex                   as KeyProxy
import qualified Hunt.Index.RTreeIndex                       as RTree
-- ----------------------------------------------------------------------------

indexTests :: [Test]
indexTests =
  -- strictness property for data-structures used in index and
  -- document table
  [ testProperty "prop_strictness_occurrences"               prop_occs

  -- strictness property for index implementations by function
  -- insert / insertList
  , testProperty "prop_strictness insert prefixtreeindex"    prop_ptix
  , testProperty "prop_strictness insert prefixtreeindex2D"  prop_ptix2d
  , testProperty "prop_strictness insert textindex"          prop_invix1
  , testProperty "prop_strictness insert numericindex"       prop_invix2
  , testProperty "prop_strictness insert dateindex"          prop_invix3
  , testProperty "prop_strictness insert geoindex"           prop_invix4
  , testProperty "prop_strictness insert geoindex rtree"     prop_insert_rtree
  , testProperty "prop_strictness insert proxy"              prop_proxy
  -- delete / deleteDocs
  , testProperty "prop_strictness delete prefixtreeindex"    prop_ptix_del
  , testProperty "prop_strictness delete prefixtreeindex2D"  prop_ptix2d_del
  , testProperty "prop_strictness delete textindex"          prop_invix1_del
  , testProperty "prop_strictness delete numericindex"       prop_invix2_del
  , testProperty "prop_strictness delete dateindex"          prop_invix3_del
  , testProperty "prop_strictness delete geoindex"           prop_invix4_del
  , testProperty "prop_strictness delete geoindex rtree"     prop_rtree_del
  , testProperty "prop_strictness delete proxy"              prop_proxy_del
  -- map
  , testProperty "prop_strictness map prefixtreeindex"       prop_ptix_map
  , testProperty "prop_strictness map prefixtreeindex2D"     prop_ptix2d_map
  , testProperty "prop_strictness map textindex"             prop_invix1_map
  , testProperty "prop_strictness map numericindex"          prop_invix2_map
  , testProperty "prop_strictness map dateindex"             prop_invix3_map
  , testProperty "prop_strictness map geoindex"              prop_invix4_map
  , testProperty "prop_strictness map geoindex rtree"        prop_rtree_map
  , testProperty "prop_strictness map proxy"                 prop_proxy_map
  -- mapMaybe
  , testProperty "prop_strictness mapMaybe prefixtreeindex"  prop_ptix_map2
  , testProperty "prop_strictness mapMaybe prefixtreeinde2d" prop_ptix2d_map2
  , testProperty "prop_strictness mapMaybe textindex"        prop_invix1_map2
  , testProperty "prop_strictness mapMaybe numericindex"     prop_invix2_map2
  , testProperty "prop_strictness mapMaybe dateindex"        prop_invix3_map2
  , testProperty "prop_strictness mapMaybe geoindex"         prop_invix4_map2
  , testProperty "prop_strictness mapMaybe geoindex rtree"   prop_rtree_map2
  , testProperty "prop_strictness mapMaybe proxy"            prop_proxy_map2
  -- unionWith
  , testProperty "prop_strictness unionWith prefixtreeindex" prop_ptix_union
  , testProperty "prop_strictness unionWith prefixtreeind2d" prop_ptix2d_union
  , testProperty "prop_strictness unionWith textindex"       prop_invix1_union
  , testProperty "prop_strictness unionWith numericindex"    prop_invix2_union
  , testProperty "prop_strictness unionWith dateindex"       prop_invix3_union
  , testProperty "prop_strictness unionWith geoindex"        prop_invix4_union
  , testProperty "prop_strictness unionWith geoindex rtree"  prop_rtree_union
  , testProperty "prop_strictness unionWith proxy"           prop_proxy_union
  ]

-- ----------------------------------------------------------------------------
-- test data structures
-- ----------------------------------------------------------------------------

prop_occs :: Property
prop_occs = monadicIO $ do
  x <- pick arbitrary :: PropertyM IO Occurrences
  assertNF' $! x

-- ----------------------------------------------------------------------------
-- index implementations: insert function
-- ----------------------------------------------------------------------------

prop_ptix :: Property
prop_ptix
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.DmPrefixTree Occurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" (toIntermediate (val::Occurrences)) Ix.empty

prop_ptix2d :: Property
prop_ptix2d
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.DmPrefixTree Occurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "11" (toIntermediate (val::Occurrences)) Ix.empty

prop_invix1 :: Property
prop_invix1
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" (toIntermediate (val::Occurrences)) Ix.empty

prop_invix2 :: Property
prop_invix2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.PrefixTreeIndexInt)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "1" (toIntermediate (val::Occurrences)) Ix.empty

prop_invix3 :: Property
prop_invix3
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.PrefixTreeIndexDate)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "2013-01-01" (toIntermediate (val::Occurrences)) Ix.empty

prop_invix4 :: Property
prop_invix4
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.PrefixTreeIndexPosition)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "1-1" (toIntermediate (val::Occurrences)) Ix.empty

prop_insert_rtree :: Property
prop_insert_rtree
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (RTree.RTreeIndex Occurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert (RTree.readPosition "1-1") (toIntermediate (val::Occurrences)) Ix.empty

prop_proxy :: Property
prop_proxy
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree Occurrences))
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> return $ Ix.insert "key" (toIntermediate (val::Occurrences)) Ix.empty

-- ----------------------------------------------------------------------------
-- index implementations: delete function
-- ----------------------------------------------------------------------------

prop_ptix_del :: Property
prop_ptix_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.DmPrefixTree Occurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "key"


prop_ptix2d_del :: Property
prop_ptix2d_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.DmPrefixTree Occurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "11"

prop_invix1_del :: Property
prop_invix1_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "key"

prop_invix2_del :: Property
prop_invix2_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.PrefixTreeIndexInt)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "1"

prop_invix3_del :: Property
prop_invix3_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.PrefixTreeIndexDate)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "2013-01-01"

prop_invix4_del :: Property
prop_invix4_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.PrefixTreeIndexPosition)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "1-1"

prop_rtree_del :: Property
prop_rtree_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (RTree.RTreeIndex Occurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete (RTree.readPosition "1-1")

prop_proxy_del :: Property
prop_proxy_del
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree Occurrences))
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_delete "key"

insert_and_delete :: forall (m :: * -> *) a.
                     (Ix.ICon a, Monad m, IndexValue (Ix.IVal a), Ix.Index a) =>
                      Ix.IKey a -> DocIdMap Positions -> m a
insert_and_delete key v
  = return $ Ix.delete docId
           $ Ix.insert key (toIntermediate (v::Occurrences))
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
    ix <- pickIx :: PropertyM IO (PIx.DmPrefixTree Occurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map "key"

prop_ptix2d_map :: Property
prop_ptix2d_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.DmPrefixTree Occurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map "11"

prop_invix1_map :: Property
prop_invix1_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map "key"

prop_invix2_map :: Property
prop_invix2_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.PrefixTreeIndexInt)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= \val -> insert_and_map_withSet "1" (val :: DocIdSet)

prop_invix3_map :: Property
prop_invix3_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.PrefixTreeIndexDate)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map_withSet "2013-01-01"

prop_invix4_map :: Property
prop_invix4_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.PrefixTreeIndexPosition)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map_withSet "1-1"

prop_rtree_map :: Property
prop_rtree_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (RTree.RTreeIndex Occurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map (RTree.readPosition "1-1")

prop_proxy_map :: Property
prop_proxy_map
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree Occurrences))
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map "key"

insert_and_map :: forall (m :: * -> *) a.
                  (Ix.ICon a, Monad m, Ix.Index a, Ix.IVal a ~ DocIdMap Positions) =>
                  Ix.IKey a -> Occurrences -> m a
insert_and_map key v
  = return $ Ix.map (DM.insert (mkDocId (1 :: Int)) (Pos.singleton 1))
           $ Ix.insert key (toIntermediate (v::Occurrences)) Ix.empty

insert_and_map_withSet :: forall (m :: * -> *) a.
                          (Ix.ICon a, Monad m, Ix.Index a, Ix.IVal a ~ DocIdSet) =>
                          Ix.IKey a -> DocIdSet -> m a
insert_and_map_withSet key v
  = return $ Ix.map (DS.union (DS.singleton $ mkDocId (1 :: Int)))
           $ Ix.insert key (toIntermediate (v::DocIdSet)) Ix.empty



-- ----------------------------------------------------------------------------
-- index implementations: mapMaybe function
-- ----------------------------------------------------------------------------

prop_ptix_map2 :: Property
prop_ptix_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.DmPrefixTree Occurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2 "key"

prop_ptix2d_map2 :: Property
prop_ptix2d_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.DmPrefixTree Occurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2 "11"

prop_invix1_map2 :: Property
prop_invix1_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2 "key"

prop_invix2_map2 :: Property
prop_invix2_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.PrefixTreeIndexInt)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2_withSet "1"

prop_invix3_map2 :: Property
prop_invix3_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.PrefixTreeIndexDate)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2_withSet "2013-01-01"

prop_invix4_map2 :: Property
prop_invix4_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.PrefixTreeIndexPosition)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2_withSet "1-1"


prop_rtree_map2 :: Property
prop_rtree_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (RTree.RTreeIndex Occurrences)
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2 (RTree.readPosition "1-1")

prop_proxy_map2 :: Property
prop_proxy_map2
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree Occurrences))
    assertNF' ix
  where
  pickIx = pick arbitrary >>= insert_and_map2 "key"

insert_and_map2 :: forall (m :: * -> *) a.
                   (Ix.ICon a, Monad m, Ix.Index a, Ix.IVal a ~ DocIdMap Positions) =>
                   Ix.IKey a -> Occurrences -> m a
insert_and_map2 key v
  = return $ Ix.mapMaybe (Just . DM.insert (mkDocId (1 :: Int)) (Pos.singleton 1))
           $ Ix.insert key (toIntermediate (v::Occurrences)) Ix.empty

insert_and_map2_withSet :: forall (m :: * -> *) a.
                           (Ix.ICon a, Monad m, Ix.Index a, Ix.IVal a ~ DocIdSet) =>
                           Ix.IKey a -> DocIdSet -> m a
insert_and_map2_withSet key v
  = return $ Ix.mapMaybe (Just . DS.union (DS.singleton $ mkDocId (1 :: Int)))
           $ Ix.insert key (toIntermediate (v::DocIdSet)) Ix.empty

-- ----------------------------------------------------------------------------
-- index implementations: unionWith function
-- ----------------------------------------------------------------------------

prop_ptix_union :: Property
prop_ptix_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.DmPrefixTree Occurrences)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary
    val2 <- pick arbitrary
    insert_and_union "key" val1 val2

prop_ptix2d_union :: Property
prop_ptix2d_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.DmPrefixTree Occurrences)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary
    val2 <- pick arbitrary
    insert_and_union "11" val1 val2

prop_invix1_union :: Property
prop_invix1_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (InvIx.InvertedIndex)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary
    val2 <- pick arbitrary
    insert_and_union "key" val1 val2

prop_invix2_union :: Property
prop_invix2_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.PrefixTreeIndexInt)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary
    val2 <- pick arbitrary
    insert_and_union_withSet "1" val1 val2

prop_invix3_union :: Property
prop_invix3_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx.PrefixTreeIndexDate)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary :: PropertyM IO DocIdSet
    val2 <- pick arbitrary :: PropertyM IO DocIdSet
    insert_and_union_withSet "2013-01-01" val1 val2

prop_invix4_union :: Property
prop_invix4_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (PIx2D.PrefixTreeIndexPosition)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary :: PropertyM IO DocIdSet
    val2 <- pick arbitrary :: PropertyM IO DocIdSet
    insert_and_union_withSet "1-1" val1 val2

prop_rtree_union :: Property
prop_rtree_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (RTree.RTreeIndex Occurrences)
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary :: PropertyM IO Occurrences
    val2 <- pick arbitrary :: PropertyM IO Occurrences
    insert_and_union (RTree.readPosition "1-1") val1 val2

prop_proxy_union :: Property
prop_proxy_union
  = monadicIO $ do
    ix <- pickIx :: PropertyM IO (KeyProxy.KeyProxyIndex Text (PIx.DmPrefixTree Occurrences))
    assertNF' ix
  where
  pickIx = do
    val1 <- pick arbitrary :: PropertyM IO Occurrences
    val2 <- pick arbitrary :: PropertyM IO Occurrences
    insert_and_union "key" val1 val2

insert_and_union :: forall (m :: * -> *) a v.
                    (Ix.ICon a, Monad m, IndexValue (DocIdMap v), Ix.Index a,
                    Ix.IVal a ~ DocIdMap v) =>
                    Ix.IKey a -> Occurrences -> Occurrences -> m a
insert_and_union key v1 v2
  = return $ Ix.unionWith (DM.union)
             (Ix.insert key (toIntermediate (v1::Occurrences)) Ix.empty)
             (Ix.insert key (toIntermediate (v2::Occurrences)) Ix.empty)

insert_and_union_withSet :: forall (m :: * -> *) a.
                            (Ix.ICon a, Monad m, Ix.Index a, Ix.IVal a ~ DocIdSet) =>
                            Ix.IKey a -> DocIdSet -> DocIdSet -> m a
insert_and_union_withSet key v1 v2
  = return $ Ix.unionWith (DS.union)
             (Ix.insert key (toIntermediate (v1::DocIdSet)) Ix.empty)
             (Ix.insert key (toIntermediate (v2::DocIdSet)) Ix.empty)

