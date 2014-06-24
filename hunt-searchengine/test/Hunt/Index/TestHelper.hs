{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TypeFamilies     #-}
module Hunt.Index.TestHelper where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId              (DocId, mkDocId)
import           Hunt.Common.DocIdMap           (DocIdMap)
import           Hunt.Common.Occurrences
import           Hunt.Common.Positions          (Positions)

import qualified Hunt.Index                     as Ix

docIdOne, docIdTwo :: DocId
docIdOne = mkDocId (1::Int)
docIdTwo = mkDocId (2::Int)

occOne, occTwo :: Occurrences
occOne = singleton docIdOne (1::Int)
occTwo = singleton docIdOne (2::Int)

-- | Generic insert test function
insertTest :: (Monad m, Ix.Index i, Eq (Ix.IVal i v), (Ix.ICon i v), Ix.IVal i v ~ DocIdMap Positions) =>
              i v -> Ix.IKey i v -> Ix.IVal i v -> m Bool
insertTest emptyIndex k v = do
  ix       <- Ix.insertM merge k v emptyIndex
  [(_,nv)] <- Ix.searchM PrefixNoCase k ix
  return $ v == nv

-- | Generic delete test function
deleteTest :: (Monad m, Ix.Index i, Eq (Ix.IVal i v), (Ix.ICon i v), Ix.IVal i v ~ DocIdMap Positions) =>
              i v -> Ix.IKey i v -> Ix.IVal i v -> DocId -> m Bool
deleteTest emptyIndex k v did = do
  ix       <- Ix.insertM merge k v emptyIndex
  [(_,nv)] <- Ix.searchM PrefixNoCase k ix
  ix'      <- Ix.deleteM did ix
  return $ v == nv && Prelude.null (Ix.toList ix')

-- | Generic Occurrence merge function
mergeTest :: (Monad m, Ix.ICon i v, Ix.Index i, (Eq (Ix.IKey i v))
             , Ix.IVal i v ~ DocIdMap Positions) =>
             i v -> Ix.IKey i v -> Occurrences -> Occurrences -> m Bool
mergeTest emptyIndex k v1 v2 = do
  mergeIx   <- Ix.insertM merge k v2 $ Ix.insert merge k v1 emptyIndex
  [(nk,nv)] <- Ix.searchM PrefixNoCase k mergeIx
  return $ nk == k && (merge v1 v2) == nv
