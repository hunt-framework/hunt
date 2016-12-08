module Fox.Index.Writer where

import           Fox.Index.Monad
import           Fox.Types

tryMerge :: IndexWriter ()
tryMerge = undefined

insertDocument :: Document -> IndexWriter ()
insertDocument doc = insertDocuments [doc]

insertDocuments :: [Document] -> IndexWriter ()
insertDocuments docs = undefined
