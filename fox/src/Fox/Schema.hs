{-# LANGUAGE BangPatterns #-}
module Fox.Schema where

import           Fox.Types

import           Data.Bits
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Vector                (Vector)
import qualified Data.Vector                as Vector
import qualified Data.Vector.Algorithms.Tim as Tim

data Schema = Schema
  {
    schemaFields     :: !(HashMap FieldName FieldType)
  , schemaFieldCount :: !Int
  } deriving (Eq, Show)

-- a strict tuple type to avoid space leak
data Pair a b = P !a !b
              deriving (Eq, Show)

-- | Helps to reduce duplicate FieldNames in memory while
-- indexing documents.
data InternedSchema = InternedSchema
  {
    ischemaFields     :: !(HashMap FieldName (Pair FieldName FieldType))
  , ischemaFieldCount :: !Int
  } deriving (Eq, Show)

type LookupFieldType = FieldName -> Maybe FieldType

-- | Insert a new field into the schema, if there is no type conflict with
-- an existing field an interned version of the 'FieldName' is returned.
insertField :: FieldName
            -> FieldType
            -> InternedSchema
            -> Either FieldType (FieldName, InternedSchema)
insertField fieldName fieldTy internedSchema
  | Just (P fieldName' fieldTy') <-
      HashMap.lookup fieldName (ischemaFields internedSchema)
  = if fieldTy == fieldTy'
    then Right (fieldName', internedSchema)
    else Left fieldTy'
  | otherwise
  = Right (fieldName, internedSchema')
  where
    internedSchema' = internedSchema {
        ischemaFields     = HashMap.insert fieldName (P fieldName fieldTy) (ischemaFields internedSchema)
      , ischemaFieldCount = ischemaFieldCount internedSchema + 1
      }

emptySchema :: Schema
emptySchema =
  Schema { schemaFields = HashMap.empty
         , schemaFieldCount = 0
         }

emptyInternedSchema :: InternedSchema
emptyInternedSchema =
  InternedSchema { ischemaFields = HashMap.empty
                 , ischemaFieldCount = 0
                 }

-- | Lookup a type for a field
lookupField :: FieldName -> Schema -> Maybe FieldType
lookupField fieldName schema =
  HashMap.lookup fieldName (schemaFields schema)

uninternSchema :: InternedSchema -> Schema
uninternSchema schema =
  Schema { schemaFields = HashMap.map (\(P _ ty) -> ty) (ischemaFields schema)
         , schemaFieldCount = ischemaFieldCount schema
         }

internSchema :: Schema -> InternedSchema
internSchema schema =
  InternedSchema { ischemaFields = HashMap.mapWithKey P (schemaFields schema)
                 , ischemaFieldCount = schemaFieldCount schema
                 }

type FieldOrds = Vector FieldName

fieldOrds :: Schema -> FieldOrds
fieldOrds schema =
  Vector.modify Tim.sort
  $ Vector.fromListN (schemaFieldCount schema) (HashMap.keys (schemaFields schema))

lookupFieldOrd :: FieldOrds -> FieldName -> Int
lookupFieldOrd fields fieldName = go 0 (Vector.length fields)
  where
    go !l !u
      | u <= l    = 0
      | otherwise =
          case compare (fields `Vector.unsafeIndex` k) fieldName of
            LT -> go (k + 1) u
            EQ -> k
            GT -> go l k
      where
        k = (u + l) `unsafeShiftR` 1

