{-# LANGUAGE ExistentialQuantification #-}

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
import           Hunt.Common.Occurrences     (Occurrences)
import           Hunt.Index

-- ------------------------------------------------------------

type IndexImplCon i v
  = ( Index i
    , IKey i v ~ Text
    , IVal i v ~ v
    , ICon i v
    , Show (i v)
    , Binary (i v)
    , Typeable (i v)
    )

-- ------------------------------------------------------------

data IndexImpl v
  = forall i. IndexImplCon i v => IndexImpl { ixImpl :: i v }

-- ------------------------------------------------------------

instance Show (IndexImpl v) where
  show (IndexImpl v) = show v

-- FIXME: not 'rnf v `seq` ()'. is it supposed to be that way?
instance NFData (IndexImpl v) where
  rnf (IndexImpl v) = v `seq` ()

-- ------------------------------------------------------------
-- Serialization

-- | FIXME: actually implement instance
instance Binary (IndexImpl v) where
  put (IndexImpl i) = put (typeOf i) >> put i
  get = error "existential types cannot be deserialized this way. Use special get' functions"

get' :: [IndexImpl Occurrences] -> Get [(Context, IndexImpl Occurrences)]
get' ts = do
  n <- get :: Get Int
  getMany' ts n

getMany' :: [IndexImpl Occurrences] -> Int -> Get [(Context, IndexImpl Occurrences)]
getMany' ts n = go [] n
  where
  go xs 0 = return $! reverse xs
  go xs i = do
    x <- liftM2 (,) get (get'' ts)
    x `seq` go (x:xs) (i-1)

get'' :: [IndexImpl Occurrences] -> Get (IndexImpl Occurrences)
get'' ts = do
  t <- get :: Get TypeRep
  case L.find (\(IndexImpl i) -> t == typeOf i) ts of
    Just (IndexImpl x) -> IndexImpl <$> get `asTypeOf` return x
    Nothing            -> error $ "Unable to load index of type: " -- ++ show t

-- ------------------------------------------------------------

-- | IndexImpl is the Wrapper for external access
--   we set the key to Text here, but allow internal
--   Key in all haskell types. For conversion we have
--   could imagine a normalization proxy implemented
--   with the KeyIndex Proxy
mkIndex :: IndexImplCon i v => i v -> IndexImpl v
mkIndex i = IndexImpl $! i

-- ------------------------------------------------------------
