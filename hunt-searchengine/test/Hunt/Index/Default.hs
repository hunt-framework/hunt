{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TypeFamilies     #-}
module Hunt.Index.Default where

import           Data.List                      (null)
import           Data.Text                      (Text, unpack)

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck.Monadic

import           Hunt.Common.BasicTypes
import qualified Hunt.Common.DocIdSet           as Set
import           Hunt.Index.Schema
import           Hunt.Index.IndexImpl           (IndexImpl(..))

import qualified Hunt.Index                     as Ix
import           Hunt.Index.Helper

-- ----------------------------------------------------------------------------
-- Testsuite for `ContextType`s and underlying `Index` implementations
--
-- Note: To test new `ContextType`s, just add the respective `ContextType` to
-- the `contextTypes` below.

-- | TestSuite for `Index` interface
tests :: [Test]
tests = concat $ map testIndex contextTypes

-- | list of `ContextType`s and a valid key for each Type
contextTypes :: [(ContextType, Text)]
contextTypes = [ (ctText,          "test")
               , (ctTextSimple,    "test")
               , (ctInt,           "1000")
               , (ctDate,          "2012-01-01")
               , (ctPosition,      "1-1")
               , (ctPositionRTree, "1-1")
               ]

-- | TestSuite for one concrete `ContextType` or `Index` implemation
testIndex :: (ContextType, Text) -> [Test]
testIndex (CType name _ _ (IndexImpl impl), key)
  = [ testProperty (mkName name "insert")     (monadicIO $ insertTest impl key)
    , testProperty (mkName name "insertList") (monadicIO $ insertListTest impl key)
    , testProperty (mkName name "delete")     (monadicIO $ deleteTest impl key)
    , testProperty (mkName name "deleteDocs") (monadicIO $ deleteDocsTest impl key)
    , testProperty (mkName name "empty")      (monadicIO $ emptyTest impl)
    , testProperty (mkName name "toList")     (monadicIO $ toListTest impl key)
    ]
  where
   mkName n t = "ContextType " ++ unpack n ++ ": " ++ t

-- ----------------------------------------------------------------------------
-- insert tests

-- | Test insert function of `Index` typeclass
insertTest :: (Ix.Index i, Monad m, Ix.ICon i) => i -> Ix.IKey i -> m Bool
insertTest impl key
  = do
    ix1 <- Ix.insertM key values impl
    res <- Ix.searchM PrefixNoCase key ix1
    checkResult [values] res
    where
      values = simpleValue1

-- | Test insertList function of `Index` typeclass
insertListTest :: (Ix.Index i, Monad m, Ix.ICon i) => i -> Ix.IKey i -> m Bool
insertListTest impl key
  = do
    ix1 <- Ix.insertListM (addKey key values) impl
    res <- Ix.searchM PrefixNoCase key ix1
    checkResult values res
    where
      values  = [simpleValue1, simpleValue2]

-- ----------------------------------------------------------------------------
-- delete tests

-- | Test delete function of 'Index' typeclass
deleteTest :: (Ix.Index i, Monad m, Ix.ICon i) => i -> Ix.IKey i -> m Bool
deleteTest impl key
  = do
    -- insert
    ix1 <- Ix.insertListM (addKey key values) impl
    rs1 <- Ix.searchM PrefixNoCase key ix1
    -- delete
    ix2 <- Ix.deleteDocsM (Set.fromList [docId1, docId2]) ix1
    rs2 <- Ix.searchM PrefixNoCase key ix2
    -- check
    ch1 <- checkResult values rs1
    ch2 <- checkResult [] rs2
    return $ ch1 && ch2
    where
      values = [simpleValue1, simpleValue2]

deleteDocsTest :: (Ix.Index i, Monad m, Ix.ICon i) => i -> Ix.IKey i -> m Bool
deleteDocsTest impl key
  = do
    -- insert
    ix1 <- Ix.insertListM (addKey key values) impl
    rs1 <- Ix.searchM PrefixNoCase key ix1
    -- delete
    ix2 <- Ix.deleteM docId1 ix1
    rs2 <- Ix.searchM PrefixNoCase key ix2
    -- check
    ch1 <- checkResult values rs1
    ch2 <- checkResult [simpleValue2] rs2
    return $ ch1 && ch2
    where
      values = [simpleValue1, simpleValue2]

-- ----------------------------------------------------------------------------
-- test other functions

-- | test `empty` function from `Index` typeclass
emptyTest :: (Ix.Index i, Monad m, Ix.ICon i) => i -> m Bool
emptyTest impl
  = do
    let ix = Ix.empty `asTypeOf` impl
    return . Data.List.null $ Ix.toList ix

-- | test `toList` function from `Index` typeclass
toListTest :: (Ix.Index i, Monad m, Ix.ICon i) => i -> Ix.IKey i -> m Bool
toListTest impl key
  = do
    ix1 <- Ix.insertListM (addKey key values) impl
    ls  <- Ix.toListM ix1
    checkResult values ls
    where
      values = [simpleValue1, simpleValue2]
