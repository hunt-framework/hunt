{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Holumbus.Index.Proxy.ContextIndex where

import           Prelude                     as P

import           Control.Monad

import           Data.Binary                 (Binary (..))
import           Data.Binary.Get
import           Data.Map.Strict             (Map)
import qualified Data.Map.Strict             as M
import           Data.Text                   (Text)
import           Data.Text.Binary            ()
import           Data.Traversable            as TV

import           Holumbus.Common
import qualified Holumbus.Index.Index        as Ix
import qualified Holumbus.Index.IndexImpl    as Impl
import           Holumbus.Index.IndexImpl    (IndexImpl)

-- ----------------------------------------------------------------------------

-- | ContextIndex stores different kinds of indexes
--   and hides their implementation. The API enforces
--   usage of a Text key.
newtype ContextIndex v
  = ContextIx { contextIx :: Map Context (Impl.IndexImpl v) }
  deriving (Show)

-- ----------------------------------------------------------------------------

get' :: [IndexImpl Occurrences] -> Get (ContextIndex Occurrences)
get' ts = liftM M.fromDistinctAscList (Impl.get' ts) >>= return . ContextIx

instance Binary v => Binary (ContextIndex v) where
  put = put . contextIx
  get = get >>= return . ContextIx

-- ----------------------------------------------------------------------------

-- | Empty ContextIndex.
empty :: ContextIndex v
empty = ContextIx $ M.empty

-- | Insert a new context.
--   Note: If context already exists this function does nothing.
insertContext' :: Impl.IndexImplCon ix v
              => Context -> ix v -> ContextIndex v -> ContextIndex v
insertContext' c ix (ContextIx m) = ContextIx $ M.insertWith (const id) c (Impl.mkIndex ix) m

insertContext :: Context -> Impl.IndexImpl v -> ContextIndex v -> ContextIndex v
insertContext c ix (ContextIx m) = ContextIx $ M.insertWith (const id) c ix m


-- | Removes context including attached index from ContextIndex.
deleteContext :: Context -> ContextIndex v -> ContextIndex v
deleteContext c (ContextIx m) = ContextIx $ M.delete c m

-- | Insert an element to one Context.
insertWithCx :: Monad m => Context -> Text -> v -> ContextIndex v -> m (ContextIndex v)
insertWithCx c w v (ContextIx m)
  = case M.lookup c m of
      Just (Impl.IndexImpl ix) -> do
        ix' <- liftM Impl.mkIndex $ Ix.insert w v ix
        return $ ContextIx $ M.insert c ix' m
      _      -> error "context does not exist"
  --where
  --adjust' (Impl.IndexImpl ix) = Impl.mkIndex $ 

{-
-- | Insert an element to a list of contexts.
insertWithCxs :: [Context] -> Text -> v -> ContextIndex v -> ContextIndex v
insertWithCxs cs w v i = foldr (\c ix -> insertWithCx c w v ix) i cs
-}

-- | Insert an element to one Context.
delete :: Monad m => DocIdSet -> ContextIndex v -> m (ContextIndex v)
delete dIds (ContextIx m)
  = liftM ContextIx $ TV.mapM adjust' m
  where
  adjust' (Impl.IndexImpl ix) = liftM Impl.mkIndex $ Ix.batchDelete dIds ix

search :: Monad m => TextSearchOp -> Text -> ContextIndex v -> m [(Context, [(Text, v)])]
search op k (ContextIx m)
  = liftM M.toList $ TV.mapM search' m
  where
  search' (Impl.IndexImpl ix) = Ix.search op k ix

-- XXX: code duplication? - see searchwithcx...
lookupRangeCx :: Monad m => Context -> Text -> Text -> ContextIndex v -> m [(Text, v)]
lookupRangeCx c k1 k2 (ContextIx m)
  = case M.lookup c m of
      Just (Impl.IndexImpl cm) -> Ix.lookupRange k1 k2 cm
      _                        -> return []

lookupRangeCxs :: Monad m => [Context] -> Text -> Text -> ContextIndex v -> m [(Context, [(Text, v)])]
lookupRangeCxs cs k1 k2 (ContextIx m)
  = P.mapM search' cs
  where
  search' c = case M.lookup c m of
      Just (Impl.IndexImpl cm) -> do
        ix <- Ix.lookupRange k1 k2 cm
        return (c, ix)
      _ -> return (c, [])

searchWithCx :: Monad m => TextSearchOp -> Context -> Text -> ContextIndex v -> m [(Text, v)]
searchWithCx op c k (ContextIx m)
  = case M.lookup c m of
      Just (Impl.IndexImpl cm) -> Ix.search op k cm
      _                        -> return []

-- | XXX we actually do not have any parallelism here at the moment
--   because everything is evalutated lazy!
searchWithCxs :: Monad m => TextSearchOp -> [Context] -> Text -> ContextIndex v -> m [(Context, [(Text, v)])]
searchWithCxs op cs k (ContextIx m)
  = P.mapM search' cs
  where
  search' c = case M.lookup c m of
      Just (Impl.IndexImpl cm) -> do
        ix <- Ix.search op k cm
        return (c, ix)
      _ -> return (c, [])

-- | search in different contexts with key already normalized in respect to each context type
searchWithCxsNormalized :: Monad m => TextSearchOp -> [(Context, Text)] -> ContextIndex v -> m [(Context, [(Text, v)])]
searchWithCxsNormalized op cks (ContextIx m)
  = P.mapM search' cks
  where
  search' (c, k) = case M.lookup c m of
      Just (Impl.IndexImpl cm) -> do
        ix <- Ix.search op k cm
        return (c, ix)
      _ -> return (c, [])

-- ----------------------------------------------------------------------------

-- | Contexts/keys of 'ContextIndex'.
contexts :: ContextIndex v -> [Context]
contexts (ContextIx m) = M.keys m

-- | Check if the context exists.
hasContext :: Context -> ContextIndex v -> Bool
hasContext c (ContextIx m) = M.member c m

-- ----------------------------------------------------------------------------
-- probably unnecessary functions

{-
deleteWithCx :: Context -> DocIdSet -> ContextIndex v -> ContextIndex v
deleteWithCx c dIds (ContextIx m)
  = case M.lookup c m of
      (Just _) -> ContextIx $ M.adjust adjust' c m
      _        -> error "context does not exist"
  where
  adjust' (Impl.IndexImpl ix) = Impl.mkIndex $ Ix.batchDelete dIds ix


deleteWithCxs :: [Context] -> DocIdSet -> ContextIndex v -> ContextIndex v
deleteWithCxs cs dIds i = foldr (\c ix -> deleteWithCx c dIds ix) i cs


lookupRange :: Text -> Text -> ContextIndex v
            -> [(Context, [(Text, v)])]
  = M.toList $ M.map range' m
  where
  range' (Impl.IndexImpl ix) = Ix.lookupRange k1 k2 ix
-}
