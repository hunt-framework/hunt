{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TypeFamilies              #-}

module Holumbus.Index.IndexImpl where

import qualified Data.List                      as L
import qualified Data.Map                       as M
import           Data.ByteString.Lazy           (ByteString)
import           Data.Binary
import           Data.Text                      (Text)
import           Control.Monad
import           Control.Applicative            ((<$>), (<*>))
import           Data.Typeable
import           Data.Typeable.Internal         (TypeRep(..), TyCon(..))
import           GHC.Fingerprint.Type           (Fingerprint(..))

import           Holumbus.Common
import           Holumbus.Index.Index

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




type ContextTypes = M.Map Text ContextMeta

data ContextMeta = CxMeta
  { cmType   :: CType
  , cmIxImpl :: IndexImpl Occurrences
  }

-- ------------------------------------------------------------

instance Show (IndexImpl v) where
  show (IndexImpl v) = show v

instance Binary Fingerprint where
  put (Fingerprint hi lo) = put hi >> put lo
  get = Fingerprint <$> get <*> get 

instance Binary TypeRep where
  put (TypeRep fp tyCon ts) = put fp >> put tyCon >> put ts 
  get = TypeRep <$> get <*> get <*> get 

instance Binary TyCon where
  put (TyCon hash package modul name) = put hash >> put package >> put modul >> put name
  get = TyCon <$> get <*> get <*> get <*> get


get' :: ContextTypes -> Get [(Context, IndexImpl Occurrences)]
get' ts = do 
          n <- get :: Get Int
          getMany' ts n

getMany' :: ContextTypes -> Int -> Get [(Context, IndexImpl Occurrences)]
getMany' ts n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- liftM2 (,) get (get'' ts)
                 x `seq` go (x:xs) (i-1)

get'' :: ContextTypes -> Get (IndexImpl Occurrences)
get'' ts = do
        t <- get :: Get TypeRep
        case L.find (\(IndexImpl i) -> t == typeOf i) (impls ts) of
          (Just (IndexImpl x)) -> IndexImpl <$> get `asTypeOf` return x
          Nothing          -> error $ "Unable to load index of type: " -- ++ show t
        where
        impls ts = L.map (cmIxImpl . snd) (M.toAscList ts)

-- | FIXME: actually implement instance
instance Binary (IndexImpl v) where
  put (IndexImpl i) = put (typeOf i) >> put i
  get = error "existential types cannot be derialized this way. Use special get' functions"

-- ------------------------------------------------------------

-- | IndexImpl is the Wrapper for external access
--   we set the key to Text here, but allow internal
--   Key in all haskell types. For conversion we have
--   could imagine a normalization proxy implemented
--   with the KeyIndex Proxy
mkIndex :: IndexImplCon i v  => i v -> IndexImpl v
mkIndex i = IndexImpl $! i
