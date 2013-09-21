{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Text.Inverted.PrefixMem
  ( Inverted
  , empty
  , fromList)
where

import           Control.Arrow
import           Control.DeepSeq

import           Data.Binary                       (Binary (..))
import           Data.Map                          (Map)
import qualified Data.Map                          as M
import           Data.Maybe
import           Data.Text                         (Text)
import qualified Data.Text                         as T

import qualified Holumbus.Data.PrefixTree          as PT

import           Holumbus.Index.Common             (Context, Occurrences,
                                                    RawResult, Textual (..),
                                                    Word)
import qualified Holumbus.Index.Common.DocIdMap    as DM
import qualified Holumbus.Index.Common.Occurrences as Occ
import           Holumbus.Index.Compression        as C
import           Holumbus.Index.TextIndex

-- ----------------------------------------------------------------------------

-- | The Index implementation type.
--   Mapping contexts to prefix-trees, which map
--   to compressed occurrences (which map documents/ids to positions).

newtype Inverted        = Inverted { indexParts :: Parts }
                          deriving (Show, Eq)

-- | The index parts are identified by a name, which should denote the context of the words.
type Parts              = Map Context Part

-- | The index part is the real inverted index. Words are mapped to their occurrences.
type Part               = PT.PrefixTree CompressedOccurrences

-- ----------------------------------------------------------------------------

instance NFData Inverted where
  rnf = rnf . indexParts

instance Binary Inverted where
  put = put . indexParts
  get = get >>= return . Inverted

-- ----------------------------------------------------------------------------

-- | New 'Index' value using the 'Inverted' implementation.
instance Index Inverted where
    type IValue Inverted = Occurrences
    type IType  Inverted = Textual

    -- Returns the number of unique words in the index.
    unique                 = sizeWords'

    -- Returns a list of all contexts avaliable in the index.
    contexts               = contexts'

    -- Returns the occurrences for every word. A potentially expensive operation.
    size                   = allWords'

    -- TODO: flip?
    lookup                 = flip lookup'

    -- Insert occurrences.
    insert                 = insertOccurrences'

    -- Delete occurrences.
    delete                 = deleteOccurrences'

    -- Delete documents completely (all occurrences).
    deleteDocs             = deleteDocsById'

    -- Merges two indexes.
    merge                  = mergeIndexes'

    -- Subtract one index from another.
    subtract               = subtractIndexes'

    {-
    -- Splitting an index by its contexts.
    splitByContexts        = map newIndex . splitByContexts' i

    -- Splitting an index by its documents.
    splitByDocuments       = map newIndex . splitByDocuments' i

    -- Splitting an index by its words.
    splitByWords           = map newIndex . splitByWords' i

    -- Update document id's (e.g. for renaming documents). If the function maps two different id's
    -- to the same new id, the two sets of word positions will be merged if both old id's are present
    -- in the occurrences for a word in a specific context.
    mapDocIds                = \f -> newIndex $ updateDocIdsX f i
    -}

    -- Convert an Index to a list. Can be used for easy conversion between different index
    -- implementations.
    toList                 = toList'

-- ----------------------------------------------------------------------------

-- | The empty 'Index'.
empty                             :: TextIndex Inverted => Inverted
empty                             = Inverted M.empty

-- | Create an Index from a list. Can be used for easy conversion between different index
fromList                          :: TextIndex Inverted => [(Context, Word, Occurrences)] -> Inverted
fromList                          = fromList'

-- ----------------------------------------------------------------------------

-- | Modify the index 'Parts'.
liftInv                           :: (Parts -> Parts) -> Inverted -> Inverted
liftInv f                         = Inverted . f . indexParts

-- | Create an empty index.
emptyInverted                     :: Inverted
emptyInverted                     = Inverted M.empty

-- | Create an index with just one word in one context.
singleton                         :: Context -> Text -> Occurrences -> Inverted
singleton c w o                   = Inverted (M.singleton c (PT.singleton (T.unpack w) (deflateOcc o)))

-- | Merge two sets of index parts.
mergeParts                        :: Parts -> Parts -> Parts
mergeParts                        = M.unionWith mergePart

-- | Merge two index parts.
mergePart                         :: Part -> Part -> Part
mergePart                         = PT.unionWith mergeDiffLists
  where
  mergeDiffLists o1 o2            = deflateOcc $
                                  Occ.merge (inflateOcc o1) (inflateOcc o2)

-- | Subtract a set of index parts from another.
subtractParts                     :: Parts -> Parts -> Parts
subtractParts                     = M.differenceWith subtractPart

-- | Subtract one index part from another.
subtractPart                      :: Part -> Part -> Maybe Part
subtractPart p1 p2                = if PT.null diffPart then Nothing else Just diffPart
  where
  diffPart                        = PT.differenceWith subtractDiffLists p1 p2
    where
    subtractDiffLists o1 o2      = if diffOcc == Occ.empty then Nothing else Just (deflateOcc diffOcc)
      where
      diffOcc                     = Occ.subtract (inflateOcc o1) (inflateOcc o2)

{-
-- | Internal split function used by the split functions from the HolIndex interface (above).
splitInternal                     :: [(Int, Inverted)] -> Int -> [Inverted]
splitInternal inp n               = allocate mergeIndexes' stack buckets
  where
  buckets                         = zipWith const (createBuckets n) stack
  stack                           = reverse (sortBy (compare `on` fst) inp)

-- | Allocates values from the first list to the buckets in the second list.
allocate                          :: (a -> a -> a) -> [(Int, a)] -> [(Int, a)] -> [a]
allocate _ _ []                   = []
allocate _ [] ys                  = map snd ys
allocate f (x:xs) (y:ys)          = allocate f xs (sortBy (compare `on` fst) (combine x y : ys))
  where
  combine (s1, v1) (s2, v2)       = (s1 + s2, f v1 v2)

-- | Create empty buckets for allocating indexes.
createBuckets                     :: Int -> [(Int, Inverted)]
createBuckets n                   = replicate n (0, emptyInverted)
-}

-- | Return a part of the index for a given context.
getPart                           :: Context -> Inverted -> Part
getPart c i                       = fromMaybe PT.empty (M.lookup c $ indexParts i)

-- ----------------------------------------------------------------------------

sizeWords'                        :: Inverted -> Int
sizeWords'                        = M.foldr ((+) . PT.size) 0 . indexParts

contexts'                         :: Inverted -> [Context]
contexts'                         = map fst . M.toList . indexParts

allWords'                         :: Inverted -> Context -> RawResult
allWords' i c                     = map (T.pack *** inflateOcc)  $ PT.toList                                  $ getPart c i

prefixCase'                       :: Inverted -> Context -> Text -> [(Text, Occurrences)]
prefixCase' i c q                 = map (T.pack *** inflateOcc)  $ PT.prefixFindWithKey (T.unpack q)          $ getPart c i

prefixNoCase'                     :: Inverted -> Context -> Text -> [(Text, Occurrences)]
prefixNoCase' i c q               = map (T.pack *** inflateOcc)  $ PT.prefixFindNoCaseWithKey (T.unpack q)    $ getPart c i

lookupCase'                       :: Inverted -> Context -> Text -> [(Text, Occurrences)]
lookupCase' i c q                 = map ((,) q .  inflateOcc)    $ maybeToList . PT.lookup (T.unpack q)       $ getPart c i

lookupNoCase'                     :: Inverted -> Context -> Text -> [(Text, Occurrences)]
lookupNoCase' i c q               = map (T.pack *** inflateOcc)  $ PT.lookupNoCase (T.unpack q)               $ getPart c i

mergeIndexes'                     :: Inverted -> Inverted -> Inverted
mergeIndexes' i1 i2               = Inverted (mergeParts (indexParts i1) (indexParts i2))

subtractIndexes'                  :: Inverted -> Inverted -> Inverted
subtractIndexes' i1 i2            = Inverted (subtractParts (indexParts i1) (indexParts i2))

insertOccurrences'                :: Context -> Text -> Occurrences -> Inverted -> Inverted
insertOccurrences' c w o          = mergeIndexes' (singleton c w o)

deleteOccurrences'                :: Context -> Text -> Occurrences -> Inverted -> Inverted
deleteOccurrences' c w o i        = subtractIndexes' i (singleton c w o)

{-
splitByContexts'                  :: Inverted -> Int -> [Inverted]
splitByContexts' (Inverted ps)    = splitInternal (map (uncurry annotate) . M.toList $ ps)
  where
  annotate c p  = let i = Inverted (M.singleton c p)
                  in (sizeWords' i, i)

splitByDocuments'                 :: Inverted -> Int -> [Inverted]
splitByDocuments' i               = let
                                    unionDocs' = M.unionWith (M.unionWith unionPos)
                                    docResults = map (\c -> resultByDocument c (allWords' i c)) (contexts' i)
                                    convert (d, cs)             = foldl' makeIndex (0, emptyInverted) (M.toList cs)
                                        where
                                        makeIndex r (c, ws)       = foldl' makeOcc r (M.toList ws)
                                          where
                                          makeOcc (rs, ri) (w, p) = (sizePos p + rs , insertOccurrences' c w (DM.singleton d p) ri)
                                    in splitInternal ( map convert $
                                                    DM.toList $
                                                    DM.unionsWith unionDocs' docResults
                                                  )

splitByWords'                     :: Inverted -> Int -> [Inverted]
splitByWords' i                   = splitInternal indexes
  where
  indexes                         = map convert $
                                    M.toList $
                                    M.unionsWith (M.unionWith Occ.merge) wordResults
    where
    wordResults                   = map (\c -> resultByWord c (allWords' i c)) (contexts' i)
    convert (w, cs)               = foldl' makeIndex (0, emptyInverted) (M.toList cs)
      where
      makeIndex (rs, ri) (c, o)   = (rs + Occ.size o, insertOccurrences' c w o ri)

updateDocIdsX                     :: (Context -> Text -> DocId -> DocId) -> Inverted -> Inverted
updateDocIdsX f (Inverted parts)
                                  = Inverted (M.mapWithKey updatePart parts)
  where
  updatePart c                    = PT.mapWithKey
                                    (\w o -> DM.foldrWithKey (updateDocument c w) DM.empty o)
  updateDocument c w d            = DM.insertWith mergePositions (f c (T.pack w) d)
    where
    mergePositions p1 p2          = deflatePos $ unionPos (inflatePos p1) (inflatePos p2)
-}
{-
updateDocIdsX'                    :: (DocId -> DocId) -> Inverted -> Inverted
updateDocIdsX' f
                                  = Inverted . M.map updatePart . indexParts
  where
  updatePart                      = PT.map updateOcc
  updateOcc                       = DM.foldWithKey updateId DM.empty
  updateId                        = DM.insert . f
-}

toList' :: Inverted -> [(Context, Text, Occurrences)]
toList' i                         = concatMap convertPart . M.toList $ indexParts i
  where convertPart (c,p)         = map (\(w, o) -> (c, T.pack w, inflateOcc o)) .
                                    PT.toList $ p

fromList'                         :: [(Context, Text, Occurrences)] -> Inverted
fromList'                         = foldl (\i (c,w,o) -> insertOccurrences' c w o i) emptyInverted

deleteDocsById'                   :: DM.DocIdSet -> Inverted -> Inverted
deleteDocsById' docIds            = liftInv $ M.mapMaybe deleteInParts
  where
  deleteInParts :: Part -> Maybe Part
  deleteInParts p
    = let p' = PT.mapMaybe deleteInPT p
      in if PT.null p'
            then Nothing
            else return p'
  deleteInPT :: CompressedOccurrences -> Maybe CompressedOccurrences
  deleteInPT occ
    = let occ' = C.differenceWithKeySet docIds occ
      in if DM.null occ'
            then Nothing
            else return occ'


lookup' :: Inverted -> Textual -> Context -> Text -> [(Text, Occurrences)]
lookup' i t = case t of
    -- Searches for words beginning with the prefix in a given context (case-sensitive).
         PrefixCase   -> prefixCase' i
    -- Searches for words beginning with the prefix in a given context (case-insensitive).
         PrefixNoCase -> prefixNoCase' i
    -- Searches for and exact word in a given context (case-sensitive).
         Case         -> lookupCase' i
    -- Searches for and exact word in a given context (case-insensitive).
         NoCase       -> lookupNoCase' i
