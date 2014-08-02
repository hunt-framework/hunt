{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies               #-}

-- ----------------------------------------------------------------------------
{- |
  Text index using the 'DocIdMap' based on the 'StringMap' implementation.
-}
-- ----------------------------------------------------------------------------

module Hunt.Index.RTreeIndex
( RTreeIndex(..)
  , readPosition
  , showPosition
)
where

import           Control.DeepSeq

import           Data.Binary                          (Binary (..))
import qualified Data.List                            as L
import           Data.Monoid                          ((<>))
import qualified Data.RTree.Strict                    as RT
import           Data.RTree.MBB
import           Data.Text                            (Text)
import qualified Data.Text                            as T (pack, unpack)
import           Data.Typeable

import           Hunt.Index
import           Hunt.Common.Occurrences              (Occurrences)
import           Hunt.Common.IntermediateValue
import           Hunt.Index.Schema.Normalize.Position (position)

import           Text.Parsec


-- ------------------------------------------------------------

-- | Index using 'Data.RTree'
newtype RTreeIndex
  = DmRT { dmRT :: RT.RTree Occurrences }
  deriving (Eq, Show, NFData, Typeable)

mkDmRT :: RT.RTree Occurrences -> RTreeIndex
mkDmRT v = DmRT $! v

-- ------------------------------------------------------------

instance Binary RTreeIndex where
  put = put . dmRT
  get = get >>= return . mkDmRT

-- ------------------------------------------------------------

instance Index RTreeIndex where
  type IKey RTreeIndex = RT.MBB
  type IVal RTreeIndex = Occurrences

  insertList kvs (DmRT rt) =
    mkDmRT $ L.foldl' (\ m' (k', v') -> RT.insertWith mergeValues k' v' m') rt (fromIntermediates kvs)

    {- same problem as in PrefixTreeIndex, the k' in kvs don't need to be unique

       mkDmRT $ RT.unionWith op rt (RT.fromList kvs)
    -}

  deleteDocs ks (DmRT rt)
    = mkDmRT $ RT.mapMaybe (diffValues ks) rt

  empty
    = mkDmRT $ RT.empty

  fromList
    = mkDmRT . RT.fromList . fromIntermediates

  toList (DmRT rt)
    = toIntermediates . RT.toList $ rt

  -- MBBs don't have any case or prefix
  search _ k (DmRT rt)
    = toIntermediates $ RT.lookupRangeWithKey k rt

  lookupRange k1 k2 (DmRT rt)
    = toIntermediates $ RT.lookupRangeWithKey (unionMBB k1 k2) rt

  unionWith op (DmRT rt1) (DmRT rt2)
    = mkDmRT $ RT.unionWith op rt1 rt2

  map f (DmRT rt)
    = mkDmRT $ fmap f rt

  -- prohibitiv expensive.
  mapMaybe f (DmRT rt)
    = mkDmRT $ RT.mapMaybe f rt

  keys (DmRT rt)
    = RT.keys rt

-- ------------------------------------------------------------

readPosition :: Text -> MBB
readPosition t
  = case parse position "" $ T.unpack t of
     Right (la,lo) -> mbb la lo la lo
     Left _        -> error "readPosition positon: invalid"

showPosition :: MBB -> Text
showPosition (MBB la lo _ _ ) = T.pack (show la) <> "-" <> T.pack (show lo)
