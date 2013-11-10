{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{--
 - this file somehow causes ghc to deadlock when not
 - compiled without cabal clean
 -
 - maybe it doesn't make so much sense to have this
 - contextindex beeing an instance of index.
 -
 - the index can be exchanged either way but the
 - change from contextindex to
 -}

module Holumbus.Index.Proxy.ContextIndex where

import           Data.Binary                  (Binary(..))
import           Data.Text.Binary             ()
import           Data.Map                     (Map)
import qualified Data.Map                     as M

import           Holumbus.Common.BasicTypes   (Context, Textual)
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
insertContext :: ContextIxCon i v => Context -> ContextIndex i v -> ContextIndex i v
insertContext c (ContextIx m) = ContextIx $ M.insertWith (const id) c Ix.empty m

-- | removes context including attached index from ContextIndex
deleteContext :: ContextIxCon i v => Context -> ContextIndex i v -> ContextIndex i v
deleteContext c (ContextIx m) = ContextIx $ M.delete c m

insert :: ContextIxCon impl v =>
          (Maybe Context, Maybe (Ix.IKey impl v)) -> Ix.IVal impl v
          -> ContextIndex impl v -> ContextIndex impl v
insert k v (ContextIx m)
    = case k of
        -- Creates new empty Context, if Context does not exist
        (Just c, Nothing) -> ContextIx $ M.insertWith (const id) c Ix.empty m
        -- Inserts new pair into index of given context
        (Just c, Just w)  -> case M.lookup c m of
            (Just _) -> ContextIx $ M.adjust (Ix.insert w v) c m
            _        -> ContextIx $ M.insertWith (const id) c (Ix.insert w v Ix.empty) m
        -- noop
        (Nothing, Nothing) -> ContextIx m
        -- Wort in alle Kontexte einfuegen
        (Nothing, Just w)  -> ContextIx $ M.map (Ix.insert w v) m

empty :: ContextIndex i v
empty = ContextIx $ M.empty

lookup :: ContextIxCon i v =>
          Textual
          -> (Maybe Context, Maybe (Ix.IKey i v))
          -> ContextIndex i v
          -> [(Context, [(Ix.IKey i v, Ix.IVal i v)])]
lookup t k (ContextIx m)
    = case k of
        (Just c,  Just w)  -> case M.lookup c m of
            (Just cm) -> [(c, Ix.search t w cm)]
            _         -> []
        (Nothing, Just w)  -> M.toList $ M.map (Ix.search t w) m
        _                  -> []

keys :: ContextIndex i v -> [Context]
keys (ContextIx m) = M.keys m

map :: (i v -> j w) -> ContextIndex i v -> ContextIndex j w
map f (ContextIx m) = ContextIx $ M.map f m
