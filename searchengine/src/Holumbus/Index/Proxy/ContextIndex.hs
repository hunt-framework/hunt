module Holumbus.Index.Proxy.ContextIndex where

import           Holumbus.Index.Common   (Context)
import           Holumbus.Index.Index    as Ix
import           Data.Map                (Map)
import qualified Data.Map                as M


newtype ContextIndex impl v = ContextIx (Map Context (impl v))
    deriving (Show)

instance Index (ContextIndex impl) where
    type IKey (ContextIndex impl)   v = (Maybe Context, Maybe (IKey impl v))
    type IToL (ContextIndex impl)   v = [(Context, IToL impl v)]
    type ICon (ContextIndex impl)   v = ( Index impl
                                        , IVal (ContextIndex impl) v ~ IVal impl v
                                        , IType (ContextIndex impl) v ~ IType impl v
                                        , ICon impl v
                                        )

    insert k v (ContextIx m)
        = case k of
            -- Creates new empty Context, if Context does not exist
            (Just c,  Nothing) -> ContextIx $ M.insertWith (const id) c empty m
            -- Inserts new pair into index of given context
            (Just c,  Just w)  -> ContextIx $ M.adjust (insert w v) c m
            -- noop
            (Nothing, Nothing) -> ContextIx m
            -- Wort in alle Kontexte einfuegen
            (Nothing, Just w)  -> ContextIx $ M.map (insert w v) m

    batchDelete ds (ContextIx m)
        = ContextIx $ M.map (batchDelete ds) m

{-        = case k of
            -- alles loeschen
            (Nothing, Nothing) -> empty
            -- in allen Kontexten w loeschen
            (Nothing, Just w)  -> ContextIx $ M.map (delete w) m
            -- einen Context c loeschen
            (Just c,  Nothing) -> ContextIx $ M.delete c m
            -- ein Wort w in einem Kontext c loeschen
            (Just c,  Just w)  -> ContextIx
                                $ M.adjust (delete w) c m
-}
    empty = ContextIx $ M.empty

    fromList xs
        = ContextIx $ foldr ins M.empty xs
          where
            ins (c, ws) m = M.insert c (fromList ws) m

    toList (ContextIx m)
        = M.toList $ M.map toList m

    search t k (ContextIx m)
        = case k of
            (Just c,  Just w)  -> case M.lookup c m of
                                    (Just cm) -> [(c, search t w cm)]
                                    _         -> []
            (Nothing, Just w)  -> M.toList $ M.map (search t w) m
            _                  -> []

    map f (ContextIx m)
        = ContextIx $ M.map (Ix.map f) m

    -- | xxx TODO implement function
    unionWith --op (ContextIx i1) (ContextIx i2)
        = undefined
