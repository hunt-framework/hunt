module Hunt.ContextIndex.Snapshot where

import qualified Data.Map.Strict as Map

import           Hunt.ContextIndex.Types
import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema

snapshot :: ContextIndex dt -> (ContextMap, ContextIndex dt)
snapshot ixx = (old, snapshot' ixx)
  where
    old = head (ciIndex ixx)

snapshot' :: ContextIndex dt -> ContextIndex dt
snapshot' ixx = ixx { ciIndex = cm : ciIndex ixx }
  where
    cm = newContextMap (ciSchema ixx)

newContextMap :: Schema -> ContextMap
newContextMap = ContextMap . Map.map (newIx . ctIxImpl . cxType)
  where
    newIx :: Ix.IndexImpl -> Ix.IndexImpl
    newIx (Ix.IndexImpl i) = Ix.mkIndex (Ix.empty `asTypeOf` i)
