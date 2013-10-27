{-# LANGUAGE UndecidableInstances #-}
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

import           Holumbus.Index.Common   (Context, Textual)
import qualified Holumbus.Index.Index    as Ix
import           Data.Map                (Map)
import qualified Data.Map                as M

newtype ContextIndex impl v = ContextIx (Map Context (impl v))
    deriving (Show)

type ContextIxCon impl v = ( Ix.Index impl
                           , Ix.ICon impl v
                           )

insert :: ContextIxCon impl v => 
          (Maybe Context, Maybe (Ix.IKey impl v)) -> 
          Ix.IVal impl v -> ContextIndex impl v -> ContextIndex impl v
insert k v (ContextIx m)
        = case k of
            -- Creates new empty Context, if Context does not exist
            (Just c,  Nothing) -> ContextIx $ M.insertWith (const id) c Ix.empty m
            -- Inserts new pair into index of given context
            (Just c,  Just w)  -> ContextIx $ M.adjust (Ix.insert w v) c m
            -- noop
            (Nothing, Nothing) -> ContextIx m
            -- Wort in alle Kontexte einfuegen
            (Nothing, Just w)  -> ContextIx $ M.map (Ix.insert w v) m

empty :: ContextIndex i v
empty = ContextIx $ M.empty

lookup :: ContextIxCon i v
          => Textual
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
