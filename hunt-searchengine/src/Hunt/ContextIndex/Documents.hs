module Hunt.ContextIndex.Documents where

import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId            (DocId)
import           Hunt.Common.Document         (Document)
import           Hunt.DocTable.HashedDocTable (Documents)
import qualified Hunt.DocTable.HashedDocTable as Documents
import qualified Hunt.DocTable                as DocTable

import Control.Monad.IO.Class
import Control.DeepSeq
import qualified Data.Vector.Unboxed          as UVector
import           Data.Word

-- |A DocTableIndex represents an on-disk DocTable. dtiDocIds is oredered strictly
-- monotone. While dtiDocInfo contains the on-disk offset and size of the document.
-- This structure allows document access with exactly one disk-seek.
data DocTableIndex =
  DTI { dtiDocIds  :: !(UVector.Vector DocId)
      , dtiDocInfo :: !(UVector.Vector (Word64, Word64))
      }

instance NFData DocTableIndex where
  rnf dt = rnf (dtiDocIds dt) `seq` rnf (dtiDocInfo dt) `seq` ()

data DocTable = DtDocs !(Documents Document)
              | DtIxed !(DocId -> IO Document) !DocTableIndex

instance NFData DocTable where
  rnf (DtDocs docs) = rnf docs `seq` ()
  rnf (DtIxed get ixed) = get `seq` rnf ixed `seq` ()

null :: Monad m => DocTable -> m Bool
null (DtDocs docs) = DocTable.null docs
null (DtIxed _ ix) = return (UVector.null (dtiDocIds ix))

size :: Monad m => DocTable -> m Int
size (DtDocs docs) = DocTable.size docs
size (DtIxed _ ix) = return (UVector.length (dtiDocIds ix))

lookup :: MonadIO m => DocId -> DocTable -> m (Maybe Document)
lookup did (DtDocs docs) = DocTable.lookup did docs
lookup did (DtIxed _ ix) = undefined

lookupByURI :: Monad m => URI -> DocTable -> m (Maybe DocId)
lookupByURI uri (DtDocs docs) = DocTable.lookupByURI uri docs
lookupByURI uri (DtIxed _ ix) = undefined

union :: MonadIO m => DocTable -> DocTable -> m DocTable
union (DtDocs docs1) (DtDocs docs2) = DtDocs <$> DocTable.union docs1 docs2
union (DtIxed _ ix1) (DtIxed _ ix2) = undefined
union _ _ = undefined

disjoint :: MonadIO m => DocTable -> DocTable -> m Bool
disjoint (DtDocs docs1) (DtDocs docs2) = DocTable.disjoint docs1 docs2
disjoint (DtIxed _ ix1) (DtIxed _ ix2) = undefined
disjoint _ _ = undefined

insert :: Monad m => Document -> DocTable -> m (DocId, DocTable)
insert doc (DtDocs docs) = do
  (did, dt) <- DocTable.insert doc docs
  return (did, DtDocs dt)
insert doc (DtIxed _ _) = undefined

update :: Monad m => DocId -> Document -> DocTable -> m DocTable
update did doc (DtDocs docs) = DtDocs <$> DocTable.update did doc docs
update did doc (DtIxed _ _ ) = undefined

instance DocTable.DocTable DocTable where
  type DValue DocTable = Document
  type Cxt m DocTable = (MonadIO m)
  null        = Hunt.ContextIndex.Documents.null
  size        = Hunt.ContextIndex.Documents.size
  lookup      = Hunt.ContextIndex.Documents.lookup
  lookupByURI = Hunt.ContextIndex.Documents.lookupByURI
  union       = Hunt.ContextIndex.Documents.union
  disjoint    = Hunt.ContextIndex.Documents.disjoint
  insert      = Hunt.ContextIndex.Documents.insert
  update      = Hunt.ContextIndex.Documents.update
