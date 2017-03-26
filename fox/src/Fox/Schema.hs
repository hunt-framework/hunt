{-# LANGUAGE BangPatterns #-}
module Fox.Schema where

import           Fox.Types

import           Data.Bits
import           Data.Foldable
import           Data.HashMap.Strict        (HashMap)
import qualified Data.HashMap.Strict        as HashMap
import           Data.Vector                (Vector, ifoldM', imapM_)
import qualified Data.Vector                as Vector
import qualified Data.Vector.Algorithms.Tim as Tim

-- a strict tuple type to avoid space leak
data Pair a b = P !a !b
              deriving (Eq, Show)

-- | Helps to reduce duplicate FieldNames in memory while
-- indexing documents.
data Schema = Schema
  {
    schemaFields     :: !(HashMap FieldName (Pair FieldName FieldType))
  , schemaFieldCount :: !Int
  } deriving (Eq, Show)

-- | Insert a new field into the schema, if there is no type conflict with
-- an existing field an interned version of the 'FieldName' is returned.
insertField :: FieldName
            -> FieldType
            -> Schema
            -> Either FieldType (FieldName, Schema)
insertField fieldName fieldTy schema
  | Just (P fieldName' fieldTy') <-
      HashMap.lookup fieldName (schemaFields schema)
  = if fieldTy == fieldTy'
    then Right (fieldName', schema)
    else Left fieldTy'
  | otherwise
  = Right (fieldName, schema')
  where
    schema' = schema {
        schemaFields     = HashMap.insert fieldName (P fieldName fieldTy) (schemaFields schema)
      , schemaFieldCount = schemaFieldCount schema + 1
      }

emptySchema :: Schema
emptySchema =
  Schema { schemaFields     = HashMap.empty
         , schemaFieldCount = 0
         }

internFieldName :: FieldName -> Schema -> Maybe (FieldName, FieldType)
internFieldName fieldName schema
  | Just (P fieldName' fieldTy) <- HashMap.lookup fieldName (schemaFields schema)
  = Just (fieldName', fieldTy)
  | otherwise
  = Nothing

-- | Lookup a type for a field
lookupFieldType :: FieldName -> Schema -> Maybe FieldType
lookupFieldType fieldName schema =
  snd <$> internFieldName fieldName schema

checkSchemaTys :: Schema -> Schema -> [(FieldName, FieldType, FieldType)]
checkSchemaTys schema1 schema2 =
  fold $ HashMap.intersectionWithKey check (schemaFields schema1) (schemaFields schema2)
  where
    check fieldName (P _ fieldTy) (P _ fieldTy')
      | fieldTy /= fieldTy' = [(fieldName, fieldTy, fieldTy')]
      | otherwise = []

-- | `FieldOrds` assignes a unique identifier to all fields
-- in a `Schema`. Fields can then referenced by their much
-- more compact identifier than their `FieldName`.
newtype FieldOrds = FieldOrds (Vector FieldName)

type FieldOrd = Int

fieldOrds :: Schema -> FieldOrds
fieldOrds schema =
  FieldOrds
  $ Vector.modify Tim.sort
  $ Vector.fromListN (schemaFieldCount schema) (HashMap.keys (schemaFields schema))

forFields_ :: Monad m => FieldOrds -> (FieldOrd -> FieldName -> m a) -> m ()
forFields_ (FieldOrds fords) f = imapM_ f fords

foldFields' :: Monad m => (a -> FieldOrd -> FieldName -> m a) -> a -> FieldOrds -> m a
foldFields' f z (FieldOrds fords) = ifoldM' f z fords

lookupFieldOrd :: FieldOrds -> FieldName -> FieldOrd
lookupFieldOrd (FieldOrds fields) fieldName = go 0 (Vector.length fields)
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
