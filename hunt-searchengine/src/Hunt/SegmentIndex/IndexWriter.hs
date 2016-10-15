{-# LANGUAGE RecordWildCards #-}
module Hunt.SegmentIndex.IndexWriter where

import           Hunt.Common.ApiDocument
import           Hunt.Common.BasicTypes
import           Hunt.Common.DocDesc       (FieldRank)
import qualified Hunt.Common.DocDesc       as DocDesc
import           Hunt.Common.DocId
import           Hunt.Common.Document
import           Hunt.Common.Occurrences   (Occurrences)
import qualified Hunt.Common.Occurrences   as Occ
import           Hunt.Index.Schema
import           Hunt.Index.Schema.Analyze
import qualified Hunt.SegmentIndex.Commit  as Commit
import           Hunt.SegmentIndex.Types

import           Control.Arrow
import qualified Data.List                 as List
import           Data.Map                  (Map)
import qualified Data.Map.Strict           as Map
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import qualified Data.Vector               as Vector

import           Prelude                   hiding (Word)


-- | Insert a batch of 'Document's.
insertList :: [ApiDocument]
           -> IndexWriter
           -> IO IndexWriter
insertList docs iw@IndexWriter{..} = do
  segmentId <- iwNewSegId

  let
    idsAndWords = [ (did, words) | (did, _, words) <- docsAndWords]
    documents   = [ doc | (_, doc, _) <- docsAndWords]
    inverted    = Map.mapWithKey (\cx _ ->
                                    Map.toAscList
                                    $ Map.fromListWith mappend
                                    $ contentForCx cx idsAndWords) iwSchema

  -- Write the documents to disk!
  Commit.writeDocuments
    iwIndexDir
    segmentId
    sortedFields
    documents

  -- Write the inverted index to disk!
  Commit.writeIndex
    iwIndexDir
    segmentId
    iwSchema
    (Map.toList inverted)

  return iw

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
                     . toDocAndWords' iwSchema
                   )
        $ docs

    -- | Computes the words and occurrences out of a list for one context
    contentForCx :: Context -> [(DocId, Words)] -> [(Word, Occurrences)]
    contentForCx cx vs
      = concatMap (invert . second (getWlForCx cx)) $ vs
      where
        invert (did, wl)
          = map (second (Occ.singleton' did)) $ Map.toList wl
        getWlForCx cx' ws'
          = Map.findWithDefault Map.empty cx' ws'
