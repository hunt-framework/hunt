{-# LANGUAGE ExistentialQuantification #-}
module Fox.Types.Document where

import           Fox.Types.DocDesc (FieldName, FieldType, FieldValue, fieldType)

import           Data.Bits
import           Data.Map          (Map)
import           Data.Map          as Map
import           Data.Word         (Word8)


newtype DocId = DocId { unDocId :: Int }
              deriving (Eq, Ord, Show)

firstDocId :: DocId
firstDocId = DocId 0

nextDocId :: DocId -> DocId
nextDocId (DocId d) = DocId (d + 1)

data Document =
  Document { docWeight :: !Float
           , docFields :: !(Map FieldName DocField)
           } deriving (Show)

newtype FieldFlags = FieldFlags Word8
                   deriving (Show)

fieldIndexable :: FieldFlags -> Bool
fieldIndexable (FieldFlags w) = w .&. 0x01 /= 0

setFieldIndexable :: FieldFlags -> FieldFlags
setFieldIndexable (FieldFlags w) = FieldFlags (w .|. 0x01)

fieldStore :: FieldFlags -> Bool
fieldStore (FieldFlags w) = w .&. 0x02 /= 0

setFieldStore :: FieldFlags -> FieldFlags
setFieldStore (FieldFlags w) = FieldFlags (w .|. 0x02)

data DocField =
  DocField { dfFlags  :: !FieldFlags
           , dfWeight :: !Float
           , dfValue  :: FieldValue
           } deriving (Show)

dfType :: DocField -> FieldType
dfType df = fieldType (dfValue df)

emptyDocument :: Document
emptyDocument = Document 0.0 Map.empty

filterStorable :: Document -> Document
filterStorable doc =
  doc { docFields = Map.filter (fieldStore . dfFlags) (docFields doc) }
