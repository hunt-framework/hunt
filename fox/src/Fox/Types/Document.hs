{-# LANGUAGE ExistentialQuantification #-}
module Fox.Types.Document where

import           Fox.Types.DocDesc   (FieldName, FieldValue)

import           Data.Bits
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Word           (Word8)

data Document =
  Document { docWeight :: !Float
           , docFields :: ![(FieldName, DocField)]
           }

newtype FieldFlags = FieldFlags Word8

fieldIndexable :: FieldFlags -> Bool
fieldIndexable (FieldFlags w) = w .&. 0x01 /= 0

setFieldIndexable :: FieldFlags -> FieldFlags
setFieldIndexable (FieldFlags w) = FieldFlags (w .|. 0x01)

fieldStore :: FieldFlags -> Bool
fieldStore (FieldFlags w) = w .&. 0x02 /= 0

setFieldStore :: FieldFlags -> FieldFlags
setFieldStore (FieldFlags w) = FieldFlags (w .|. 0x02)

data FieldType = FieldType

data DocField =
  DocField { fieldFlags :: !FieldFlags
           , fieldBoost :: !Float
           , fieldValue :: FieldValue
           }

emptyDocument :: Document
emptyDocument = Document 0.0 []
