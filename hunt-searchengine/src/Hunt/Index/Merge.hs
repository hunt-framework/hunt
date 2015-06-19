module Hunt.Index.Merge (gMerge) where

import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix

import           Data.Monoid
import           Data.Typeable
import           Unsafe.Coerce

-- | TODO: find a non hacky way
gMerge :: Ix.IndexImpl -> Ix.IndexImpl -> Ix.IndexImpl
gMerge (Ix.IndexImpl ix1) (Ix.IndexImpl ix2)
  | typeOf ix1 == typeOf ix2
    = Ix.mkIndex (Ix.unionWith mappend ix1 (unsafeCoerce ix2))
  | otherwise
    = error "gMerge: Index type not equal."
