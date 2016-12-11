module Fox.Index.Writer where

import           Fox.Analyze         (Analyzer, runAnalyzer)
import           Fox.Index.Monad
import           Fox.Types

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Key
import qualified Data.Set

tryMerge :: IndexWriter ()
tryMerge = undefined

insertDocument :: Document -> IndexWriter ()
insertDocument doc = insertDocuments [doc]

insertDocuments :: [Document] -> IndexWriter ()
insertDocuments docs = do
  analyzer <- askAnalyzer

  forWithKey_ docs $ \docNum doc -> do
    forWithKey_ (docFields doc) $ \field value -> do
      let tokens = runAnalyzer analyzer field (fieldValue value)
      undefined
  undefined
