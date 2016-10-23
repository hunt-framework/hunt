{-# LANGUAGE RecordWildCards #-}
module Hunt.SegmentIndex.IndexWriter where

import           Hunt.Common.ApiDocument
import           Hunt.Common.BasicTypes
import           Hunt.Common.DocDesc                (FieldRank)
import qualified Hunt.Common.DocDesc                as DocDesc
import           Hunt.Common.DocId
import           Hunt.Common.Document
import           Hunt.Common.Occurrences            (Occurrences)
import qualified Hunt.Common.Occurrences            as Occ
import           Hunt.Index.Schema
import           Hunt.Index.Schema.Analyze
import qualified Hunt.SegmentIndex.Commit           as Commit
import qualified Hunt.SegmentIndex.Descriptor       as IxDescr
import           Hunt.SegmentIndex.Types
import           Hunt.SegmentIndex.Types.Index
import           Hunt.SegmentIndex.Types.SegmentId
import           Hunt.SegmentIndex.Types.SegmentMap (SegmentMap)
import qualified Hunt.SegmentIndex.Types.SegmentMap as SegmentMap

import           Control.Arrow
import qualified Control.Monad.Parallel             as Par
import qualified Data.List                          as List
import           Data.Map                           (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Set                           (Set)
import qualified Data.Set                           as Set
import           Data.Traversable
import qualified Data.Vector                        as Vector

import           Prelude                            hiding (Word)

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
newSegment indexDirectory genSegId schema docs = do

  segmentId <- genSegId

  let
    idsAndWords = [ (did, words) | (did, _, words) <- docsAndWords]
    inverted    = Map.mapWithKey (\cx _ ->
                                    Map.toAscList
                                    $ Map.fromListWith mappend
                                    $ contentForCx cx idsAndWords) schema

  -- write the documents to disk and keep
  -- a mapping from DocId -> Offset
  Commit.writeDocuments
    indexDirectory
    segmentId
    sortedFields
    [ doc | (_, doc, _) <- docsAndWords ]

  -- write the terms to disk and remember
  -- for each word where its occurrences
  -- are stored
  termInfos <- Commit.writeIndex
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

  return ( segmentId
         , Segment { segNumDocs     = 0
                   , segNumTerms    = 0
                   , segDeletedDocs = mempty
                   , segDelGen      = 0
                   , segSchema      = schema
                   , segTermIndex   = Map.fromDistinctAscList cxMap
                   }
         )
  where
    -- collect all field names in the document
    -- descriptions so we can build an efficient
    -- mapping @Field -> FieldRank@.
    fields :: Set Field
    fields = Set.unions
             . List.map (Set.fromList . DocDesc.fields . adDescr)
             $ docs

    sortedFields :: Vector.Vector Field
    sortedFields = Vector.fromListN
                   (Set.size fields)
                   (Set.toAscList fields)

    -- convert ApiDocuments to Documents, delete null values,
    -- and break index data into words by applying the scanner
    -- given by the schema spec for the appropriate contexts
    docsAndWords :: [(DocId, Document, Words)]
    docsAndWords
      = List.zipWith (\i (d, ws) -> (DocId i, d, ws)) [0..]
        . List.map ( (\ (d, _dw, ws) -> (d, ws))
                     . toDocAndWords' schema
                   )
        $ docs

    -- | Computes the words and occurrences out of a list for one context
    contentForCx :: Context -> [(DocId, Words)] -> [(Word, Occurrences)]
    contentForCx cx vs
      = concatMap (invert . second (getWlForCx cx)) vs
      where
        invert (did, wl)
          = map (second (Occ.singleton' did)) $ Map.toList wl
        getWlForCx cx' ws'
          = Map.findWithDefault Map.empty cx' ws'

type CommitError = String

commit :: IndexWriter -> SegmentIndex -> Either CommitError SegmentIndex
commit IndexWriter{..} si@SegmentIndex{..} =
  return $! si { siSegments = SegmentMap.union iwNewSegments siSegments
                              -- TODO: check for write conflicts
               , siSchema = iwSchema
                            -- TODO: check for write conflicts
               }

close :: IndexWriter -> SegmentIndex -> Either CommitError SegmentIndex
close ixwr segix = do

  let
    removeIfZero :: Int -> a -> Maybe Int
    removeIfZero a _b = case a - 1 of
                          x | x <= 0    -> Nothing
                            | otherwise -> Just x

  segix' <- commit ixwr segix
  return segix' {
    siSegRefs = SegmentMap.differenceWith
                removeIfZero
                (siSegRefs segix)
                (iwSegments ixwr)
    }
