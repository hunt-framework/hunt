{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies              #-}

-- ----------------------------------------------------------------------------
{- |
  Heterogeneous indexes using @ExistentialQuantification@.

  Indexes have to implement the 'Index' type class as well as obey other constraints defined in
  'IndexImplCon'.

  /Note/: Due to the nature of @ExistentialQuantification@, deserialization is tricky because the
  concrete implementation is not known.
  This is circumvented by storing the type representation of the index. Deserialization is possible
  if there is a matching type representation in the set of available types.
  This requires the use of the custom get functions.
-}
-- ----------------------------------------------------------------------------

module Hunt.Index.IndexImpl where

import           Control.Applicative         ((<$>))
import           Control.DeepSeq
import           Control.Monad

import           Data.Binary
import qualified Data.List                   as L
import           Data.Text                   (Text)
import           Data.Text.Binary            ()
import           Data.Typeable
import           Data.Typeable.Binary        ()

import           Hunt.Common.BasicTypes
import           Hunt.Index

-- ------------------------------------------------------------

-- | Constraint for index implementations.
type IndexImplCon i v
  = ( Index i
    , IKey i v ~ Text
    , ICon i v
    , Show (i v)
    , IndexValue v
    , Binary v
    , Binary (i v)
    , Typeable (i v)
    )

-- ------------------------------------------------------------

-- | Index using @ExistentialQuantification@ to allow heterogeneous index containers.
data IndexImpl
  = forall i v. IndexImplCon i v => IndexImpl { ixImpl :: i v }

-- ------------------------------------------------------------

instance Show IndexImpl where
  show (IndexImpl v) = show v

-- FIXME: not 'rnf v `seq` ()'. is it supposed to be that way?
instance NFData IndexImpl where
  rnf (IndexImpl v) = v `seq` ()

-- ------------------------------------------------------------
-- Serialization

-- | FIXME: actually implement instance
instance Binary IndexImpl where
  put (IndexImpl i) = put (typeOf i) >> put i
  get = error "existential types cannot be deserialized this way. Use special get' functions"

-- TODO: refactor

-- | Deserialize a set of 'IndexImpl's. Requires a set of available index implementations.
--
--   /Note/: This will fail if a used index implementation is not provided.
gets' :: [IndexImpl] -> Get [(Context, IndexImpl)]
gets' ts = do
  n <- get :: Get Int
  go [] n
  where
  go xs 0 = return $! reverse xs
  go xs i = do
    x <- liftM2 (,) get (get' ts)
    x `seq` go (x:xs) (i-1)

-- | Deserialize an 'IndexImpl'. Requires a set of available index implementations.
--
--   /Note/: This will fail if a used index implementation is not provided.
get' :: [IndexImpl] -> Get (IndexImpl)
get' ts = do
  t <- get :: Get TypeRep
  case L.find (\(IndexImpl i) -> t == typeOf i) ts of
    Just (IndexImpl x) -> IndexImpl <$> get `asTypeOf` return x
    Nothing            -> error $ "Unable to load index of type: " -- ++ show t

-- ------------------------------------------------------------

-- | Wrap an index using @ExistentialQuantification@ to allow heterogeneous containers.
mkIndex :: IndexImplCon i v => i v -> IndexImpl
mkIndex i = IndexImpl $! i

-- ------------------------------------------------------------
