{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Hunt.Common.IntermediateValue where

import           Control.Arrow           (second)
import           Control.DeepSeq

import           Data.Aeson
import           Data.Binary             (Binary)
import qualified Data.List               as L

import           Hunt.Common.Occurrences (Occurrences)
import qualified Hunt.Common.Occurrences as Occ
import           Hunt.Common.DocIdSet    (DocIdSet (..))
import qualified Hunt.Common.DocIdSet    as DS
import qualified Hunt.Common.DocIdMap    as DM

-- This type represents the interface for a value to the engine
-- To have an easy first implementaton the intermediate is
-- basically the same as Occurrences, but that can be adjusted later
newtype IntermediateValue = IntermediateValue
  { unIntermediate :: Occurrences
  }
  deriving (Show, Eq)

mkIntermediateValue :: Occurrences -> IntermediateValue
mkIntermediateValue o = IntermediateValue $! o

instance NFData IntermediateValue where
  rnf (IntermediateValue occ) = rnf occ

instance ToJSON IntermediateValue where
  toJSON x = toJSON (fromIntermediate x :: Occurrences)

fromIntermediates :: IndexValue u => [(x, IntermediateValue)] -> [(x, u)]
fromIntermediates xs = L.map (second fromIntermediate) xs

fromScoredIntermediates :: IndexValue u => [(x, (s, IntermediateValue))] -> [(x, (s,u))]
fromScoredIntermediates xs = L.map (\(x,u) -> (x, second fromIntermediate $ u)) xs

toIntermediates :: IndexValue u => [(x,u)] -> [(x, IntermediateValue)]
toIntermediates xs = L.map (second toIntermediate) xs

class (Binary x, NFData x) => IndexValue x where
  toIntermediate   :: x -> IntermediateValue
  fromIntermediate :: IntermediateValue -> x
  mergeValues      :: x -> x -> x
  diffValues       :: DocIdSet -> x -> Maybe x

instance IndexValue Occurrences where
  toIntermediate   x = mkIntermediateValue x
  fromIntermediate x = unIntermediate $! x
  mergeValues      = Occ.merge
  diffValues s m   = let z = Occ.diffWithSet m s in
                     if Occ.null z then Nothing else Just z

mapToSet :: Occurrences -> DocIdSet
mapToSet = DS.fromList . (map fst) . DM.toList

setToMap :: DocIdSet -> Occurrences
setToMap s = Occ.merges $ map (\did -> Occ.singleton did 1) $ DS.toList s

instance IndexValue DocIdSet where
  toIntermediate     = mkIntermediateValue . setToMap
  fromIntermediate   = mapToSet . fromIntermediate
  mergeValues        = DS.union
  diffValues s1 s2   = let r = DS.difference s2 s1 in
                       if DS.null r then Nothing else Just r
