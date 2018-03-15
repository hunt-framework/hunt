module Fox.Types.DocDesc where

import           Data.ByteString     (ByteString)
import           Data.Hashable
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Int            (Int64)
import           Data.String
import           Data.Text           (Text)

-- Additional to the text for the field name we store
-- its hash. This way we don't need to compute the
-- repeatedly when looking up in a hash map.
data FieldName = FieldName !Int {-# UNPACK #-}!Text
               deriving (Show)

instance Eq FieldName where
  FieldName hash1 name1 == FieldName hash2 name2 =
    hash1 == hash2 && name1 == name2

instance Ord FieldName where
  compare (FieldName _ name1) (FieldName _ name2) =
    compare name1 name2

instance Hashable FieldName where
  hashWithSalt s (FieldName h _) = hashWithSalt s h

instance IsString FieldName where
  fromString s = let t = fromString s
                 in FieldName (hash t) t

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

instance Semigroup DocDesc where
  (<>) = union

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
