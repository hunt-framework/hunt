{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Rank2Types                 #-}
module Hunt.ContextIndex (
    -- * Construction
    empty

    -- * Contexts and Schema
  , insertContext
  , deleteContext
  , contexts
  , contextsM
  , defaultContexts
  , hasContext
  , hasContextM
  , schema

    -- * Queries
  , lookupRangeCx
  , searchWithCx
  , searchWithCxSc
  , lookupRangeCxSc
  , lookupAllWithCx

    -- * Insert\/Delete Documents
  , insertSegment
  , insertList
                                 -- XXX: these functions should be internal
                                 -- we export them to be able to test them
                                 -- is there a bedder approach to achieve this?
--  , createDocTableFromPartition  -- only used in tests
--  , unionDocTables               -- only used in tests
  , modifyWithDescription
  , delete
  , deleteDocsByURI
  , member

    -- * Documents
  , lookupDocumentByURI
  , lookupDocument
  , selectDocuments

  , ContextIndex

  , status
  ) where

import           Hunt.Common.BasicTypes
import qualified Hunt.Common.DocDesc       as DocDesc
import           Hunt.Common.DocId
import           Hunt.Common.DocIdMap      (DocIdMap)
import qualified Hunt.Common.DocIdMap      as DocIdMap
import           Hunt.Common.DocIdSet      (DocIdSet)
import qualified Hunt.Common.DocIdSet      as DocIdSet
import           Hunt.Common.Document      as Doc
import           Hunt.Common.SegmentMap    (SegmentId (..))
import qualified Hunt.Common.SegmentMap    as SegmentMap
import           Hunt.ContextIndex.Status
import           Hunt.ContextIndex.Types
import           Hunt.DocTable             (DocTable)
import qualified Hunt.DocTable             as DocTable
import qualified Hunt.Index                as Ix
import qualified Hunt.Index.IndexImpl      as Ix
import           Hunt.Index.Schema
import           Hunt.Scoring.Score
import           Hunt.Scoring.SearchResult
import           Hunt.Segment              (Segment)
import qualified Hunt.Segment              as Segment

import qualified Control.Monad.Parallel    as Par
import qualified Data.List                 as List
import qualified Data.Map.Strict           as Map
import           Data.Maybe
import           Data.Set                  (Set)
import           Data.Text                 (Text)
import           Data.Traversable

empty :: DocTable dt => ContextIndex dt
empty
  = ContextIndex { ciSegments      = SegmentMap.empty
                 , ciSchema        = mempty
                 , ciNextSegmentId = SegmentId 1
                 }

-- | Inserts a new `Context` with `ContextSchema` into the `ContextIndex`.
insertContext :: Context
              -> Ix.IndexImpl
              -> ContextSchema
              -> ContextIndex dt
              -> ContextIndex dt
insertContext cx _ix s ixx
  = ixx { ciSchema = Map.insertWith (const id) cx s (ciSchema ixx) }

-- | Removes a `Context` from the index.
deleteContext :: Context -> ContextIndex dt -> ContextIndex dt
deleteContext cx ixx
  = ixx { ciSegments = fmap (Segment.deleteContext cx) (ciSegments ixx)
        , ciSchema   = Map.delete cx (ciSchema ixx)
        }

-- | Returns any `Context` which is searched by default.
defaultContexts :: ContextIndex dt -> [Context]
defaultContexts
  = Map.keys . Map.filter cxDefault . ciSchema

-- | Returns all contexts in the index.
contexts :: ContextIndex dt -> [Context]
contexts
  = Map.keys . ciSchema

-- | See `contexts`.
contextsM :: Monad m => ContextIndex dt -> m [Context]
contextsM
  = return . contexts

-- | Checks for `Context` existence.
hasContext :: Context -> ContextIndex dt -> Bool
hasContext cx
  = Map.member cx . ciSchema

-- | See `hasContext`.
hasContextM :: Monad m => Context -> ContextIndex dt -> m Bool
hasContextM cx
  = return . hasContext cx

-- | Returns the index `Schema`.
schema :: ContextIndex dt -> Schema
schema = ciSchema

--   This is more efficient than using fold and with 'insert'.
-- | Insert multiple documents and words.
insertList :: (Par.MonadParallel m, Applicative m, DocTable dt)
           => [(DocTable.DValue dt, Words)]
           -> ContextIndex dt
           -> m (ContextIndex dt)
insertList docAndWords ixx
  = do newSeg <- Segment.fromDocsAndWords (ciSchema ixx) docAndWords
       insertSegment newSeg ixx

-- | Inserts a segment into the index. Assigns a `SegmentId` to the `Segment`.
insertSegment :: (Monad m, DocTable dt) => Segment dt -> ContextIndex dt -> m (ContextIndex dt)
insertSegment seg ixx =
  return $! ixx { ciSegments =
                      SegmentMap.insert (ciNextSegmentId ixx) seg (ciSegments ixx)
                , ciNextSegmentId = succ (ciNextSegmentId ixx)
                }

-- | Modify the descirption of a document and add words
--   (occurrences for that document) to the index.
modifyWithDescription :: (Par.MonadParallel m, Applicative m, DocTable dt)
                      => Score
                      -> Description
                      -> Words
                      -> DocId
                      -> ContextIndex dt
                      -> m (ContextIndex dt)
modifyWithDescription weight descr wrds dId ixx
    = do Just doc <- lookupDocument dId ixx -- TODO: dangerous
         ixx'     <- delete' (DocIdSet.singleton dId) ixx
         insertList [(mergeDescr doc, wrds)] ixx'
  where
      -- M.union is left-biased
      -- flip to use new values for existing keys
      -- no flip to keep old values
      --
      -- Null values in new descr will remove associated attributes
    mergeDescr
      = wrap . Doc.update (updateWeight . updateDescr) . unwrap
      where
        updateWeight d
          | weight == noScore = d
          | otherwise         = d {wght = weight}

        updateDescr d           = -- trc "updateDescr res=" $
          d {desc = DocDesc.deleteNull $
                    flip DocDesc.union d' descr'
            }
          where
            d'     = -- trc "updateDescr old=" $
              desc d
            descr' = -- trc "updateDescr new=" $
              descr

-- | Search query in a context.
searchWithCx :: Par.MonadParallel m
             => TextSearchOp
             -> Context
             -> Text
             -> ContextIndex dt
             -> m [(Text, SearchResult)]
searchWithCx op cx w ix
  = lookupIndex cx ix merge (Ix.searchM op w)

searchWithCxSc :: Par.MonadParallel m
               => TextSearchOp
               -> Context
               -> Text
               -> ContextIndex dt
               -> m [(Text, (Score, SearchResult))]
searchWithCxSc op cx w ix
  = lookupIndex cx ix merge (Ix.searchMSc op w)

lookupRangeCx :: Par.MonadParallel m
              => Context
              -> Text
              -> Text
              -> ContextIndex dt
              -> m [(Text, SearchResult)]
lookupRangeCx c k1 k2 ix
  = lookupIndex c ix merge (Ix.lookupRangeM k1 k2)

lookupRangeCxSc :: Par.MonadParallel m
                => Context
                -> Text
                -> Text
                -> ContextIndex dt
                -> m [(Text, (Score, SearchResult))]
lookupRangeCxSc c k1 k2 ix
  = lookupIndex c ix merge (Ix.lookupRangeMSc k1 k2)

lookupAllWithCx :: Par.MonadParallel m
                => Context
                -> ContextIndex dt
                -> m [(Text, SearchResult)]
lookupAllWithCx c ix
  = lookupIndex c ix merge Ix.toListM

-- | lookup an index by a context and then search this index for a word
--   result is always a list of values.
--
--   This pattern is used in all search variants
lookupIndex :: (Par.MonadParallel m, Ix.HasSearchResult r)
            => Context
            -> ContextIndex dt
            -> ([[r]] -> [r])
            -> (forall i . Ix.IndexImplCon i => i -> m [r])
            -> m [r]
lookupIndex cx ixx mrg search
  = do rx <- mapIxsP (Segment.searchSegment cx search) ixx
       return (mrg rx)
{-# INLINE lookupIndex #-}

-- | Merge function for `SearchResult`s.
merge :: (Ord a, Monoid b) => [[(a, b)]] -> [(a, b)]
merge
  = Map.toList . Map.unionsWith mappend . fmap Map.fromList
{-# INLINE merge #-}

lookupDocumentByURI :: (Par.MonadParallel m, DocTable dt)
                    => URI
                    -> ContextIndex dt
                    -> m (Maybe DocId)
lookupDocumentByURI docUri ixx
  = do dx <- mapIxsP (Segment.lookupDocumentByURI docUri) ixx
       return
         . listToMaybe
         . catMaybes $ dx

lookupDocument :: (Par.MonadParallel m, DocTable dt)
               => DocId
               -> ContextIndex dt
               -> m (Maybe (DocTable.DValue dt))
lookupDocument dId ixx
  = do dx <- mapIxsP (Segment.lookupDocument dId) ixx
       return
         . listToMaybe
         . catMaybes $ dx

selectDocuments :: (Par.MonadParallel m, Applicative m, DocTable dt)
                => DocIdSet
                -> ContextIndex dt
                -> m (DocIdMap (DocTable.DValue dt))
selectDocuments dIds ixx
  = do dx <- mapIxsP (Segment.selectDocuments dIds) ixx
       return (DocIdMap.unionsWith undefined dx)

-- | Is the document part of the index?
member :: (Par.MonadParallel m, Applicative m, DocTable dt)
       => URI
       -> ContextIndex dt
       -> m Bool
member u ixx = do
  mems <- mapIxsP (DocTable.lookupByURI u . Segment.segDocs) ixx
  return (List.any isJust mems)

-- | Delete a set of documents by 'DocId'.
delete :: (Par.MonadParallel m, DocTable dt)
       => DocIdSet
       -> ContextIndex dt
       -> m (ContextIndex dt)
delete dIds ixx
    | DocIdSet.null dIds = return ixx
    | otherwise          = delete' dIds ixx

delete' :: (Par.MonadParallel m, DocTable dt)
        => DocIdSet
        -> ContextIndex dt
        -> m (ContextIndex dt)
delete' dIds ixx
  = do sm <- for (ciSegments ixx) (return . Segment.deleteDocs dIds)
       return ixx { ciSegments = sm }

-- | Delete a set of documents by 'URI'.
deleteDocsByURI :: (Par.MonadParallel m, Applicative m, DocTable dt)
                => Set URI
                -> ContextIndex dt
                -> m (ContextIndex dt)
deleteDocsByURI us ixx
  = do sx <- for (ciSegments ixx) (Segment.deleteDocsByURI us)
       return ixx { ciSegments = sx }

mapIxsP :: Par.MonadParallel m => (Segment dt -> m a) -> ContextIndex dt -> m [a]
mapIxsP f
  = Par.mapM f . fmap snd . SegmentMap.toList  . ciSegments
{-# INLINE mapIxsP #-}
