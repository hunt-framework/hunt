module Fox.Index.Writer where

import           Fox.Index.Monad
import           Fox.Types

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Set

tryMerge :: IndexWriter ()
tryMerge = undefined

insertDocument :: Document -> IndexWriter ()
insertDocument doc = insertDocuments [doc]

insertDocuments :: [Document] -> IndexWriter ()
insertDocuments docs = undefined
  where
    schema :: HashMap FieldName FieldType
    schema = undefined
