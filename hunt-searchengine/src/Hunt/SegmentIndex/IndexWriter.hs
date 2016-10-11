module Hunt.SegmentIndex.IndexWriter where

import           Hunt.Common.ApiDocument
import           Hunt.Common.BasicTypes
import qualified Hunt.Common.DocDesc     as DocDesc
import           Hunt.Common.Document
import           Hunt.SegmentIndex.Types

import qualified Data.List               as List
import           Data.Map                (Map)
import qualified Data.Map.Strict         as Map
import           Data.Set                (Set)
import qualified Data.Set                as Set


-- | Insert a batch of 'Document's.
insertList :: [ApiDocument]
           -> IndexWriter
           -> IO IndexWriter
insertList docs iw = do
  undefined

  where
    -- collect all field names in the document
    -- descriptions so we can build an efficient
    -- mapping @Field -> FieldRank@.
    fields :: Set Field
    fields = Set.unions
             . List.map (Set.fromList . DocDesc.fields . adDescr)
             $ docs
