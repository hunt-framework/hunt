{-# LANGUAGE ExistentialQuantification #-}
module Hunt.Index.IndexValueTests where

import           Data.Text                            (Text)

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.Providers.HUnit
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck

import qualified Hunt.Common.Occurrences              as Occ
import           Hunt.Common.Occurrences              (Occurrences, singleton)
import           Hunt.Common.DocIdSet                 (DocIdSet)
import           Hunt.Common.IntermediateValue

import           Hunt.Index.Helper

-- ----------------------------------------------------------------------------
-- Testsuite for `IntermediateValue` index value instances

tests :: [Test]
tests = map (\ivt@(IVT n _) -> testCase n $ assertTrue ivt) values
  where
  assertTrue ivt = assertEqual "" True $ mergeTest ivt

data IndexValueTest
  = forall v. (IndexValue v, Eq v) => IVT { name :: String, ivt :: v }

values :: [IndexValueTest]
values = [ IVT "Occurrences" (fromIntermediate simpleValue1 :: Occurrences)
         , IVT "DocIdSet"    (fromIntermediate simpleValue1 :: DocIdSet)
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
    from = fromIntermediate

    v2 = from simpleValue1b `asTypeOf` v1
    v3 = from simpleValue2 `asTypeOf` v1

    mergeAsOcc :: forall v. IndexValue v => v -> v -> Occurrences
    mergeAsOcc i1 i2 = Occ.merge (fromInt i1) (fromInt i2)

    fromInt :: forall v. IndexValue v => v -> Occurrences
    fromInt i = fromIntermediate . toIntermediate $ i

diffTest = undefined

conversionTest :: IndexValueTest -> Bool
conversionTest (IVT _ v) = v == (fromIntermediate . toIntermediate $ v)
