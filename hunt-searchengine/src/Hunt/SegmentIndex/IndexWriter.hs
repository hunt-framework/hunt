{-# LANGUAGE RecordWildCards #-}
module Hunt.SegmentIndex.IndexWriter where

import           Hunt.Common.ApiDocument
import           Hunt.Common.BasicTypes
import qualified Hunt.Common.DocDesc                as DocDesc
import           Hunt.Common.DocId
import           Hunt.Common.DocIdSet               (DocIdSet)
import qualified Hunt.Common.DocIdSet               as DocIdSet
import           Hunt.Common.Document
import           Hunt.Common.Occurrences            (Occurrences)
import qualified Hunt.Common.Occurrences            as Occ
import           Hunt.Index.Schema
import           Hunt.Index.Schema.Analyze
import qualified Hunt.SegmentIndex.Store.TermInfos  as Store
import qualified Hunt.SegmentIndex.Store.Documents  as Store
import qualified Hunt.SegmentIndex.Descriptor       as IxDescr
import           Hunt.SegmentIndex.Types
import           Hunt.SegmentIndex.Types.Generation
import           Hunt.SegmentIndex.Types.Index
import           Hunt.SegmentIndex.Types.SegmentId
import           Hunt.SegmentIndex.Types.SegmentMap (SegmentMap)
import qualified Hunt.SegmentIndex.Types.SegmentMap as SegmentMap

import           Control.Arrow
import           Control.Monad.Error.Class
import qualified Data.List                          as List
import           Data.Map                           (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Monoid
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Traversable
import qualified Data.Vector                        as Vector

import           Prelude                            hiding (Word)

deleteDocuments :: DocIdSet -> Segment -> Segment
deleteDocuments dids seg =
  seg { segDelGen      = nextGeneration (segDelGen seg)
      , segDeletedDocs = dids <> segDeletedDocs seg
      }

-- | Insert a batch of 'Document's.
insertList :: [ApiDocument]
           -> IndexWriter
           -> IO IndexWriter
insertList docs iw@IndexWriter{..} = do

  newSegments <- newSegment
                 iwIndexDir
                 iwNewSegId
                 iwSchema
                 docs

  {-
  newSegments <- Par.mapM (newSegment
                            iwIndexDir
                            iwNewSegId
                            iwSchema
                          )
    $ partitionByLength 50 docs
  -}

  return iw { iwNewSegments =
                SegmentMap.union
                (SegmentMap.fromList [newSegments])
                iwNewSegments
            }
  where
    partitionByLength :: Int -> [a] -> [[a]]
    partitionByLength n xs0 = go xs0
      where
        go [] = []
        go xs = case List.splitAt n xs of
                  (ys, zs) -> ys : go zs

newSegment :: FilePath
           -> IO SegmentId
           -> Schema
           -> [ApiDocument]
           -> IO (SegmentId, Segment)
newSegment indexDirectory newSegId schema docs =
  let
    idsAndWords :: [(DocId, Words)]
    idsAndWords =
      [ (did, words_)
      | (did, _, words_) <- docsAndWords
      ]

    inverted :: Map Context [(Word, Occurrences)]
    inverted =
      Map.mapWithKey (\cx _ ->
                         Map.toAscList
                         $ Map.fromListWith mappend
                         $ contentForCx cx idsAndWords
                     ) schema
  in do
    segmentId <- newSegId
    -- write the documents to disk and keep
    -- a mapping from DocId -> Offset
    Store.writeDocuments
      indexDirectory
      segmentId
      sortedFields
      [ doc | (_, doc, _) <- docsAndWords ]

    Store.writeFieldInfos
      indexDirectory
      segmentId
      sortedFields

    -- write the terms to disk and remember
    -- for each word where its occurrences
    -- are stored
    termInfos <- Store.writeIndex
                indexDirectory
                segmentId
                schema
                (Map.toList inverted)

    -- construct a lookup optimized index
    -- for the contexts
    cxMap <- for termInfos $ \(cx, termInfo) ->
      case IxDescr.textIndexDescr of
        IxDescr.IndexDescriptor repr builder -> do
          ix <- IxDescr.runBuilder builder
                $ List.map (first repr) termInfo
          return (cx, IndexRepr repr ix)

    let
      segment :: Segment
      segment =
        Segment { segNumDocs     = 0
                , segDeletedDocs = DocIdSet.empty
                , segDelGen      = generationZero
                , segTermIndex   = Map.fromDistinctAscList cxMap
                }

    segment `seq` return (segmentId, segment)
  where
    -- collect all field names in the document
    -- descriptions so we can build an efficient
    -- mapping @Field -> FieldRank@.
    fields :: Set Field
    fields =
      Set.unions
      . List.map (Set.fromList . DocDesc.fields . adDescr)
      $ docs

    sortedFields :: Vector.Vector Field
    sortedFields =
      Vector.fromListN
      (Set.size fields)
      (Set.toAscList fields)

    -- convert ApiDocuments to Documents, delete null values,
    -- and break index data into words by applying the scanner
    -- given by the schema spec for the appropriate contexts
    docsAndWords :: [(DocId, Document, Words)]
    docsAndWords =
      List.zipWith (\i (d, ws) -> (DocId i, d, ws)) [0..]
      . List.map ( (\ (d, _dw, ws) -> (d, ws))
                    . toDocAndWords' schema
                 )
      $ docs

    -- | Computes the words and occurrences out of a list for one context
    contentForCx :: Context -> [(DocId, Words)] -> [(Word, Occurrences)]
    contentForCx cx vs =
      concatMap (invert . second (getWlForCx cx)) vs
      where
        invert (did, wl)
          = map (second (Occ.singleton' did)) $ Map.toList wl
        getWlForCx cx' ws'
          = Map.findWithDefault Map.empty cx' ws'

-- | If no conflict occurred the resulting @SegmentIndex@
-- contains any new and modified @Segment@s.
commit :: IndexWriter -> SegmentIndex -> Commit SegmentIndex
commit IndexWriter{..} segmentIndex@SegmentIndex{..} =
  let
    -- checks for conflcts on two segments
    checkConflict :: SegmentId -> Segment -> Segment -> [Conflict]
    checkConflict sid s1 s2 =
      -- currently only the delete generations can be changed
      -- so if we have modifed 'Segment's here they always conflict.
      [ ConflictDelete sid
      | segDelGen s1 /= segDelGen s2
      ]

    conflicts :: [Conflict]
    conflicts =
      List.concat
      $ SegmentMap.elems
      $ SegmentMap.intersectionWithKey checkConflict siSegments
      $ SegmentMap.intersection iwSegments iwModSegments

    mergedSegments :: SegmentMap Segment
    mergedSegments =
      SegmentMap.unionWith (\new _old -> new) iwModSegments
      $ SegmentMap.union iwModSegments siSegments

  in case conflicts of
       [] -> return $! segmentIndex { siSegments = mergedSegments }
       _  -> throwError conflicts

-- | Close the @IndexWriter@:
--     - Checks for conflicts and merges all new and modified @Segment@s.
--     - Decreases the reference counts for all retained @Segment@s.
close :: IndexWriter -> SegmentIndex -> Commit SegmentIndex
close indexWriter segmentIndex =
  let
    removeIfZero :: Int -> a -> Maybe Int
    removeIfZero a _b =
      case a - 1 of
        x | x <= 0    -> Nothing
          | otherwise -> Just x

    decRefCount :: SegmentMap Int -> SegmentMap a -> SegmentMap Int
    decRefCount ax bx =
      SegmentMap.differenceWith removeIfZero ax bx

  in do
    segmentIndex' <- commit indexWriter segmentIndex
    return $! segmentIndex' {
      -- decrease the reference count so we can delete
      -- obsolete 'Segment's
      siSegRefs = decRefCount
                  (siSegRefs segmentIndex')
                  (iwSegments indexWriter)
      }
