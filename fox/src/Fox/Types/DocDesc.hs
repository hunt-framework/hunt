module Fox.Types.DocDesc where

import           Data.ByteString     (ByteString)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Int            (Int64)
import           Data.Text           (Text)

type Field = Text

data FieldValue = FV_Int  !Int64
                | FV_Float !Float
                | FV_Text !Text
                | FV_Json !ByteString
                | FV_Binary !ByteString
                | FV_Null
                deriving (Eq, Show)

newtype DocDesc = DocDesc { unDesc :: HashMap Field FieldValue }
                deriving (Eq, Show)

instance Monoid DocDesc where
  mempty  = empty
  mappend = union

mkDocDesc :: HashMap Field FieldValue -> DocDesc
mkDocDesc fields = DocDesc fields

null :: DocDesc -> Bool
null (DocDesc dd) = HashMap.null dd

empty :: DocDesc
empty = mkDocDesc HashMap.empty

size :: DocDesc -> Int
size (DocDesc dd) = HashMap.size dd

fields :: DocDesc -> [Field]
fields (DocDesc dd) = HashMap.keys dd

union :: DocDesc -> DocDesc -> DocDesc
union (DocDesc d1) (DocDesc d2) = mkDocDesc $ HashMap.union d1 d2
