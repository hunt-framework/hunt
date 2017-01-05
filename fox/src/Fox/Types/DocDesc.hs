module Fox.Types.DocDesc where

import           Data.ByteString     (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Int            (Int64)
import           Data.Text           (Text)

type FieldName = Text

data FieldValue = FV_Int  !Int64
                | FV_Float !Float
                | FV_Text !Text
                | FV_Json !ByteString
                -- ^ @ByteString@ contains valid JSON
                | FV_Binary !ByteString
                -- ^ @ByteString@ contains any data
                | FV_Null
                deriving (Eq, Show)

data FieldType = FT_Int
               | FT_Float
               | FT_Text
               | FT_Json
               | FT_Binary
               | FT_Null
               deriving (Eq, Show)

fieldType :: FieldValue -> FieldType
fieldType FV_Int{}    = FT_Int
fieldType FV_Float{}  = FT_Float
fieldType FV_Text{}   = FT_Text
fieldType FV_Json{}   = FT_Json
fieldType FV_Binary{} = FT_Binary
fieldType FV_Null{}   = FT_Null

newtype DocDesc = DocDesc { unDesc :: HashMap FieldName FieldValue }
                deriving (Eq, Show)

instance Monoid DocDesc where
  mempty  = empty
  mappend = union

mkDocDesc :: HashMap FieldName FieldValue -> DocDesc
mkDocDesc = DocDesc

null :: DocDesc -> Bool
null (DocDesc dd) = HashMap.null dd

empty :: DocDesc
empty = mkDocDesc HashMap.empty

size :: DocDesc -> Int
size (DocDesc dd) = HashMap.size dd

fields :: DocDesc -> [FieldName]
fields (DocDesc dd) = HashMap.keys dd

union :: DocDesc -> DocDesc -> DocDesc
union (DocDesc d1) (DocDesc d2) = mkDocDesc $ HashMap.union d1 d2
