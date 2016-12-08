module Fox.Types.Document where

import           Fox.Types.DocDesc (DocDesc)

data Document = Document {
    docDesc   :: !DocDesc
  , docWeight :: !Float
  }

emptyDocument :: Document
emptyDocument = Document mempty 0.0
