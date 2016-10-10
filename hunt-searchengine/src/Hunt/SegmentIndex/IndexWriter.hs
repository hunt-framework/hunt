module Hunt.SegmentIndex.IndexWriter where

import           Hunt.Common.BasicTypes
import qualified Hunt.ContextIndex          as CIx
import           Hunt.SegmentIndex.Document
import           Hunt.SegmentIndex.Types

import           Data.Map                   (Map)
import qualified Data.Map.Strict            as Map
import           Data.Set                   (Set)
import qualified Data.Set                   as Set


-- | Insert a batch of 'Document's.
insertList :: [Document]
           -> IndexWriter
           -> IO IndexWriter
insertList docs iw = do
  undefined

  where
    -- select all fields of all documents
    -- if there are fields with same name
    -- but different type we keep the type
    -- of the first occurrence.
    fields :: Map FieldName FieldType
    fields = Map.unionsWith (\old _ -> old)
             . fmap (Map.map fieldType . docFields)
             $ docs
