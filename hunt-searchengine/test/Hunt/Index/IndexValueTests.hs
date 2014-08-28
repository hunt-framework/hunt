{-# LANGUAGE ExistentialQuantification #-}
module Hunt.Index.IndexValueTests where

import           Data.Maybe

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                           hiding (Test)

import qualified Hunt.Common.Occurrences              as Occ
import           Hunt.Common.Occurrences              (Occurrences)
import qualified Hunt.Common.DocIdSet                 as Set
import           Hunt.Common.DocIdSet                 (DocIdSet)
import           Hunt.Common.IntermediateValue

import           Hunt.Index.Helper

-- ----------------------------------------------------------------------------
-- Testsuite for `IntermediateValue` index value instances
-- Note: To add tests for new `IndexValue` implementations, extend the `values`
--       list

-- | List of tests
tests :: [Test]
tests = concat $ map testValue values

testValue :: IndexValueTest -> [Test]
testValue iv@(IVT n _)
  = [ testCase (mkLabel "merge"  ) (assertEqual "" True $ mergeTest iv)
    , testCase (mkLabel "diff"   ) (assertEqual "" True $ diffTest iv)
    , testCase (mkLabel "from-to") (assertEqual "" True $ conversionTest iv)
    ]
  where
   mkLabel t = "IndexValue " ++ n ++ ": " ++ t


-- | Existential type to enable generic tests
data IndexValueTest
  = forall v. (IndexValue v, Eq v) => IVT { name :: String, ivt :: v }

-- | list of all tested `IndexValue` implementations wrapped in the
--   existential `IndexValueTest` type.
--   Extend list to add more implementations to test suite.
values :: [IndexValueTest]
values = [ IVT "Occurrences" (fromIntermediate simpleValue1 :: Occurrences)
--         , IVT "DocIdSet"    (fromIntermediate simpleValue1 :: DocIdSet)
         ]

-- | merge test for `IndexValue` implementation.
mergeTest :: IndexValueTest -> Bool
mergeTest (IVT _ v1)
  = let merge1 = mergeValues v1 v2
        merge2 = mergeValues v1 v3

        check1 = mergeAsOcc v1 v2 == fromInt merge1
        check2 = mergeAsOcc v1 v3 == fromInt merge2
    in
    check1 && check2
  where
    v2 = from simpleValue1b `asTypeOf` v1
    v3 = from simpleValue2 `asTypeOf` v1

    mergeAsOcc :: forall v. IndexValue v => v -> v -> Occurrences
    mergeAsOcc i1 i2 = Occ.merge (fromInt i1) (fromInt i2)

-- | diff test for `IndexValue` implementation
diffTest :: IndexValueTest -> Bool
diffTest (IVT _ v1)
  = let diff1 = diffValues set1 v2
        diff2 = diffValues set2 (fromJust diff1)

        check1 = diffAsOcc set1 v2
        check2 = diffAsOcc set2 check1
    in
	check1 == fromInt (fromJust diff1) && Occ.null check2 && isNothing diff2
  where
    v2   = from complexValues `asTypeOf` v1
    set1 = Set.singleton docId1
    set2 = Set.singleton docId2
    diffAsOcc set d = Occ.diffWithSet (fromInt d) set

-- | converstion from and to tests for `IndexValue` implementation
conversionTest :: IndexValueTest -> Bool
conversionTest (IVT _ v) = v == (fromIntermediate . toIntermediate $ v)

-- ----------------------------------------------------------------------------
-- Helper

fromInt :: forall v. IndexValue v => v -> Occurrences
fromInt i = fromIntermediate . toIntermediate $ i

from :: forall x. IndexValue x => IntermediateValue -> x
from = fromIntermediate
