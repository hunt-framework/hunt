{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TypeFamilies     #-}
module Hunt.Index.TestHelper where

import           Data.List                      (intersect, null)
import           Data.Text                      (Text, unpack)

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.QuickCheck.Monadic

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId              (DocId, mkDocId)
import qualified Hunt.Common.DocIdSet           as Set
import           Hunt.Common.Occurrences
import           Hunt.Common.IntermediateValue
import           Hunt.Index.Schema
import           Hunt.Index.IndexImpl           (IndexImpl(..))

import qualified Hunt.Index                     as Ix

-- ----------------------------------------------------------------------------
-- Testsuite for `ContextType`s and underlying `Index` implementations

-- | TestSuite for `Index` interface
indexTests :: [Test]
indexTests = concat $ map testIndex contextTypes

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
testIndex :: (ContextType,Text) -> [Test]
testIndex (CType name _ _ (IndexImpl impl), key)
  = [ testProperty (unpack name ++ ": insert")     (monadicIO $ insertTest impl key)
    , testProperty (unpack name ++ ": insertList") (monadicIO $ insertListTest impl key)
    , testProperty (unpack name ++ ": delete")     (monadicIO $ deleteTest impl key)
    , testProperty (unpack name ++ ": deleteDocs") (monadicIO $ deleteDocsTest impl key)
    , testProperty (unpack name ++ ": empty")      (monadicIO $ emptyTest impl)
    , testProperty (unpack name ++ ": toList")     (monadicIO $ toListTest impl key)
    ]

-- ----------------------------------------------------------------------------
-- `Index` test helpers

docId1 :: DocId
docId1 = mkDocId (1::Int)

docId2 :: DocId
docId2 = mkDocId (2::Int)

fromDocId :: DocId -> IntermediateValue
fromDocId docId = toIntermediate $ singleton docId 1

simpleValue :: Int -> IntermediateValue
simpleValue i = toIntermediate $ singleton (mkDocId i) i

simpleValue1 :: IntermediateValue
simpleValue1 = simpleValue 1

simpleValue2 :: IntermediateValue
simpleValue2 = simpleValue 2

checkResult :: Monad m => [IntermediateValue] -> [(x, IntermediateValue)] -> m Bool
checkResult vs res = return $ vs == (vs `intersect` map snd res)

addKey :: x -> [IntermediateValue] -> [(x, IntermediateValue)]
addKey key = map (\v -> (key, v))

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
