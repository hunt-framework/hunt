{-# LANGUAGE BangPatterns #-}
module Hunt.ContextIndex.Documents where

import Hunt.Common.DocIdSet (DocIdSet)
import qualified Hunt.Common.DocIdSet as DocIdSet
import Hunt.Common.DocIdMap (DocIdMap)
import qualified Hunt.Common.DocIdMap as DocIdMap
import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId            (DocId)
import           Hunt.Common.Document         (Document)
import           Hunt.DocTable.HashedDocTable (Documents)
import qualified Hunt.DocTable.HashedDocTable as Documents
import qualified Hunt.DocTable                as DocTable

import Data.Bits
import           Control.Arrow
import           Control.DeepSeq
import           Control.Monad.IO.Class

import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed          as UVector
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Fusion.Bundle.Monadic as MBundle
import qualified Data.Vector.Fusion.Stream.Monadic as Stream
import qualified Data.Vector.Fusion.Util as Fusion

import           Data.Word

-- |A DocTableIndex represents an on-disk DocTable. dtiDocIds is oredered strictly
-- monotone. While dtiDocInfo contains the on-disk offset and size of the document.
-- This structure allows document access with exactly one disk-seek.
newtype DocTableIndex =
  DTI { dtiDocInfo :: UVector.Vector (DocId, Word64, Word64) }

instance NFData DocTableIndex where
  rnf (DTI x) = rnf x

data DocTable = DtDocs !(Documents Document)
              | DtIxed !(Word64 -> Word64 -> IO Document) !DocTableIndex

instance NFData DocTable where
  rnf (DtDocs docs) = rnf docs `seq` ()
  rnf (DtIxed get ixed) = get `seq` rnf ixed `seq` ()

empty :: DocTable
empty = DtDocs DocTable.empty

null :: Monad m => DocTable -> m Bool
null (DtDocs docs) = DocTable.null docs
null (DtIxed _ ix) = return (UVector.null (dtiDocInfo ix))

size :: Monad m => DocTable -> m Int
size (DtDocs docs) = DocTable.size docs
size (DtIxed _ ix) = return (UVector.length (dtiDocInfo ix))

lookup :: MonadIO m => DocId -> DocTable -> m (Maybe Document)
lookup did (DtDocs docs) = DocTable.lookup did docs
lookup did (DtIxed get ix) = lookupIxed did get ix

lookupByURI :: Monad m => URI -> DocTable -> m (Maybe DocId)
lookupByURI uri (DtDocs docs) = DocTable.lookupByURI uri docs
lookupByURI uri (DtIxed _ ix) = return Nothing

union :: MonadIO m => DocTable -> DocTable -> m DocTable
union (DtDocs docs1) (DtDocs docs2) = DtDocs <$> DocTable.union docs1 docs2
union dt@(DtIxed _ ix1) (DtIxed _ ix2) = return dt
union dt _ = return dt

disjoint :: MonadIO m => DocTable -> DocTable -> m Bool
disjoint (DtDocs docs1) (DtDocs docs2) = DocTable.disjoint docs1 docs2
disjoint (DtIxed _ ix1) (DtIxed _ ix2) = return True
disjoint _ _ = return True

insert :: Monad m => Document -> DocTable -> m (DocId, DocTable)
insert doc (DtDocs docs) = second DtDocs <$> DocTable.insert doc docs
insert doc dt@(DtIxed _ _)  = return (undefined, dt)

update :: Monad m => DocId -> Document -> DocTable -> m DocTable
update did doc (DtDocs docs) = DtDocs <$> DocTable.update did doc docs
update did doc dt@(DtIxed _ _ ) = return dt

delete :: Monad m => DocId -> DocTable -> m DocTable
delete did (DtDocs docs) = DtDocs <$> DocTable.delete did docs
delete did dt@(DtIxed _ _)  = return dt

difference :: Monad m => DocIdSet -> DocTable -> m DocTable
difference dids (DtDocs docs) = DtDocs <$> DocTable.difference dids docs
difference dids dt@(DtIxed _ _ ) = return dt

map :: Monad m => (Document -> Document) -> DocTable -> m DocTable
map f (DtDocs docs) = DtDocs <$> DocTable.map f docs
map f x@(DtIxed _ _ ) = return x

filter :: Monad m => (Document -> Bool) -> DocTable -> m DocTable
filter f (DtDocs docs) = DtDocs <$> DocTable.filter f docs
filter f x@(DtIxed _ _) = return x

restrict :: Monad m => DocIdSet -> DocTable -> m DocTable
restrict dids (DtDocs docs) = DtDocs <$> DocTable.restrict dids docs
restrict dids (DtIxed get ix) = return $ DtIxed get (restrictIxed dids ix)

toMap :: MonadIO m => DocTable -> m (DocIdMap Document)
toMap (DtDocs docs) = DocTable.toMap docs
toMap (DtIxed get ix) =
  liftIO $ UVector.foldM' (\m (did, offset, size) -> do doc <- get offset size
                                                        return (DocIdMap.insert did doc m)
                           ) DocIdMap.empty (dtiDocInfo ix)

docIds :: Monad m => DocTable -> m DocIdSet
docIds (DtDocs docs) = DocTable.docIds docs
docIds (DtIxed _ ix) = return dids
  where
    dids = DocIdSet.fromDistinctAscList
      . fmap (\(d, _, _) -> d) $ UVector.toList (dtiDocInfo ix)

instance DocTable.DocTable DocTable where
  type DValue DocTable = Document
  type Cxt m DocTable = (MonadIO m)
  null        = Hunt.ContextIndex.Documents.null
  size        = Hunt.ContextIndex.Documents.size
  lookup      = Hunt.ContextIndex.Documents.lookup
  lookupByURI = Hunt.ContextIndex.Documents.lookupByURI
  union       = Hunt.ContextIndex.Documents.union
  disjoint    = Hunt.ContextIndex.Documents.disjoint
  insert      = Hunt.ContextIndex.Documents.insert
  update      = Hunt.ContextIndex.Documents.update
  delete      = Hunt.ContextIndex.Documents.delete
  difference  = Hunt.ContextIndex.Documents.difference
  map         = Hunt.ContextIndex.Documents.map
  filter      = Hunt.ContextIndex.Documents.filter
  restrict    = Hunt.ContextIndex.Documents.restrict
  toMap       = Hunt.ContextIndex.Documents.toMap
  docIds      = Hunt.ContextIndex.Documents.docIds
  empty       = Hunt.ContextIndex.Documents.empty


lookupIxed :: MonadIO m
           => DocId
           -> (Word64 -> Word64 -> IO Document)
           -> DocTableIndex
           -> m (Maybe Document)
lookupIxed did get dti =
  case lookupDocId cmp did (dtiDocInfo dti) of
    Just (_, offset, size) -> liftIO (Just <$> get offset size)
    Nothing -> return Nothing
  where
    cmp (x, _, _) y = compare x y

data I a b =  I1 !a !b

restrictIxed :: DocIdSet
             -> DocTableIndex
             -> DocTableIndex
restrictIxed dids dti =
  DTI (GV.unstream dti'')
  where
    dti'  = MBundle.fromVector (dtiDocInfo dti)
    dids' = MBundle.elements $ MBundle.fromList (DocIdSet.toList dids)
    dti'' = MBundle.fromStream (intersect cmp (MBundle.elements dti') dids') (MBundle.size dti')
    cmp = \(did1, _, _) did2 -> compare did1 did2

    {-# INLINE intersect #-}
    intersect cmp (Stream.Stream next1 s) (Stream.Stream next2 s')
      = Stream.Stream next (I1 s s')
      where
        next (I1 s1 s2) = do
          r1 <- next1 s1
          case r1 of
            Stream.Yield x s1' -> do
              r2 <- next2 s2
              case r2 of
                Stream.Yield y s2' -> do
                  case cmp x y of
                    EQ -> return $ Stream.Yield x (I1 s1' s2')
                    LT -> return $ Stream.Skip (I1 s1' s2)
                    GT -> return $ Stream.Skip (I1 s1 s2')
                Stream.Skip s2' -> return $ Stream.Skip (I1 s1 s2')
                Stream.Done -> return Stream.Done
            Stream.Skip s1' -> return $ Stream.Skip (I1 s1' s2)
            Stream.Done -> return Stream.Done

lookupDocId :: UVector.Unbox a
            => (a -> b -> Ordering)
            -> b
            -> UVector.Vector a
            -> Maybe a
lookupDocId cmp x v = loop 0 (UVector.length v)
  where loop !l !u
          | u <= l = Nothing
          | otherwise =
              case cmp (v `UVector.unsafeIndex` k) x of
                LT -> loop (k + 1) u
                EQ -> Just (v `UVector.unsafeIndex` k)
                GT -> loop l k
          where
            !k = (u + l) `unsafeShiftR` 1
{-# INLINE lookupDocId #-}
