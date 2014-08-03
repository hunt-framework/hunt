{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds  #-}
{-# LANGUAGE TypeFamilies     #-}
module Hunt.Index.TestHelper where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId              (DocId, mkDocId)
import           Hunt.Common.Occurrences
import           Hunt.Common.IntermediateValue

import qualified Hunt.Index                     as Ix

docIdOne, docIdTwo :: DocId
docIdOne = mkDocId (1::Int)
docIdTwo = mkDocId (2::Int)

occOne, occTwo :: Occurrences
occOne = singleton docIdOne (1::Int)
occTwo = singleton docIdOne (2::Int)

-- | Generic insert test function
insertTest :: (Monad m, Ix.Index i, Eq (Ix.IVal i), (Ix.ICon i), IndexValue (Ix.IVal i)) =>
              i -> Ix.IKey i -> Ix.IVal i -> m Bool
insertTest emptyIndex k v = do
  ix       <- Ix.insertM k (toIntermediate v) emptyIndex
  [(_,nv)] <- Ix.searchM PrefixNoCase k ix
  return $ v == (fromIntermediate nv)

-- | Generic delete test function
deleteTest :: (Monad m, Ix.Index i, Eq (Ix.IVal i), (Ix.ICon i), IndexValue (Ix.IVal i)) =>
              i -> Ix.IKey i -> Ix.IVal i -> DocId -> m Bool
deleteTest emptyIndex k v did = do
  ix       <- Ix.insertM k (toIntermediate v) emptyIndex
  [(_,nv)] <- Ix.searchM PrefixNoCase k ix
  ix'      <- Ix.deleteM did ix
  return $ v == (fromIntermediate nv) && Prelude.null (Ix.toList ix')

-- | Generic Occurrence merge function
mergeTest :: (Monad m, Ix.ICon i, Ix.Index i, (Eq (Ix.IKey i )), IndexValue (Ix.IVal i)) =>
             i -> Ix.IKey i-> Occurrences -> Occurrences -> m Bool
mergeTest emptyIndex k v1 v2 = do
  mergeIx   <- Ix.insertM k (toIntermediate v2) $ Ix.insert k (toIntermediate v2) emptyIndex
  [(nk,nv)] <- Ix.searchM PrefixNoCase k mergeIx
  return $ nk == k && (merge v1 v2) == (fromIntermediate nv)

