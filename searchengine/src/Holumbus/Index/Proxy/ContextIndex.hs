{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Holumbus.Index.Proxy.ContextIndex where

import           Data.Binary                  (Binary(..))
import           Data.Text.Binary             ()
import           Data.Map                     (Map)
import qualified Data.Map                     as M

import           Holumbus.Common
import qualified Holumbus.Index.Index         as Ix

-- ----------------------------------------------------------------------------

newtype ContextIndex impl v
    = ContextIx (Map Context (impl v))
    deriving (Show)

type ContextIxCon impl v
    = ( Ix.Index impl
      , Ix.ICon impl v
      )

-- ----------------------------------------------------------------------------

instance (Binary (impl v), Binary v) => Binary (ContextIndex impl v) where
  put (ContextIx i) = put i
  get = get >>= return . ContextIx

-- ----------------------------------------------------------------------------

-- | insert new context into context index.
--   note: If context already exists this function behaves like Data.Map insert:
--   the index stored behind the context gets overwriten by an empty index
--   XXX: either the comment or the implementation is wrong :P
--   XXX: so which behaviour do we want?
insertContext :: ContextIxCon i v => Context -> ContextIndex i v -> ContextIndex i v
insertContext c (ContextIx m) = ContextIx $ M.insertWith (const id) c Ix.empty m

-- | removes context including attached index from ContextIndex
deleteContext :: ContextIxCon i v => Context -> ContextIndex i v -> ContextIndex i v
deleteContext c (ContextIx m) = ContextIx $ M.delete c m

-- | insert element to one Context
--   XXX creates context if not exists - is that what we want?
insertWithCx :: ContextIxCon impl v
             => Context -> Ix.IKey impl v -> Ix.IVal impl v
             -> ContextIndex impl v -> ContextIndex impl v
insertWithCx c w v (ContextIx m) 
  = case M.lookup c m of
      (Just _) -> ContextIx $ M.adjust (Ix.insert w v) c m
      _        -> ContextIx $ M.insertWith (const id) c (Ix.insert w v Ix.empty) m

-- | insert element to list of contexts
insertWithCxs :: ContextIxCon impl v
              => [Context] -> Ix.IKey impl v -> Ix.IVal impl v
              -> ContextIndex impl v -> ContextIndex impl v
insertWithCxs cs w v i = foldr (\c ix -> insertWithCx c w v ix) i cs

-- | insert element to all contexts
insert :: ContextIxCon impl v
       => Ix.IKey impl v -> Ix.IVal impl v
       -> ContextIndex impl v -> ContextIndex impl v
insert w v (ContextIx m) = ContextIx $ M.map (Ix.insert w v) m

-- | Make empty ContextIndex 
empty :: ContextIndex i v
empty = ContextIx $ M.empty

search :: ContextIxCon i v
       => Ix.ISearchOp i v 
       -> Ix.IKey i v
       -> ContextIndex i v
       -> [(Context, [(Ix.IKey i v, Ix.IVal i v)])]
search op k (ContextIx m) = M.toList $ M.map (Ix.search op k) m


searchWithCx :: ContextIxCon i v
             => Ix.ISearchOp i v 
             -> Context
             -> Ix.IKey i v
             -> ContextIndex i v
             -> [(Context, [(Ix.IKey i v, Ix.IVal i v)])]
searchWithCx op c k (ContextIx m) 
  = case M.lookup c m of
      (Just cm) -> [(c, Ix.search op k cm)]
      _         -> []

-- | keys of contextindex a.k.a contexts
-- XXX: bedder name? contexts?
keys :: ContextIndex i v -> [Context]
keys (ContextIx m) = M.keys m

-- | map over context index
map :: (i v -> j w) -> ContextIndex i v -> ContextIndex j w
map f (ContextIx m) = ContextIx $ M.map f m
