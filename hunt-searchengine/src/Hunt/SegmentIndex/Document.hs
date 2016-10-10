module Hunt.SegmentIndex.Document where

import           Hunt.Common.BasicTypes
import           Hunt.Index.Schema
import           Hunt.Scoring.Score

import           Data.Aeson
import           Data.Map               (Map)
import qualified Data.Map.Strict        as Map
import           Data.Text              (Text)

type FieldName = Text

type FieldValue = Value

type FieldType = ContextSchema

data Field =
  Field { fieldWeight :: !Score
        , fieldValue  :: !FieldValue
        , fieldType   :: !FieldType
        , fieldStored :: !Bool
        }

data Document =
  Document { docFields :: !(Map FieldName Field)
           , docWeight :: !Score
           }

emptyDocument :: Document
emptyDocument = Document Map.empty defScore

intField :: Int -> Field
intField i = Field { fieldWeight = noScore
                   , fieldValue  = toJSON i
                   , fieldType   = def { cxType = ctInt }
                   , fieldStored = False
                   }

textField :: Text -> Field
textField s = Field { fieldWeight = noScore
                    , fieldValue  = toJSON s
                    , fieldType   = def
                    , fieldStored = False
                    }
