module Hunt.Index.Merge (gMerge) where

import qualified Hunt.Index as Ix
import qualified Hunt.Index.IndexImpl as Ix

import           Data.List as List
import           Data.Monoid
import           Data.Typeable
import           Unsafe.Coerce

-- | TODO: find a non hacky way
gMerge' :: Ix.IndexImpl -> Ix.IndexImpl -> Ix.IndexImpl
gMerge' (Ix.IndexImpl ix1) (Ix.IndexImpl ix2)
  = if typeOf ix1 == typeOf ix2
    then Ix.mkIndex (Ix.unionWith mappend ix1 (unsafeCoerce ix2))
    else error "gMerge: Index type not equal."

-- | A generic merge function for any index type
gMerge :: [Ix.IndexImpl] -> Ix.IndexImpl
gMerge
  = List.foldr1 gMerge'
