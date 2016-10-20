{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternGuards              #-}
{-# LANGUAGE TypeFamilies               #-}
module Hunt.Common.DocDesc
where

import           Prelude             hiding (lookup)

import           Control.Arrow       (second)
import           Control.DeepSeq
import           Control.Monad       (mzero)

import           Data.Aeson          (FromJSON (..), ToJSON (..), Value (..),
                                      decode, encode)
import           Data.Binary         (Binary (..))
import           Data.ByteString     (ByteString)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromMaybe)
import qualified Data.Scientific     as Scientific
import           Data.String
import           Data.Text           (Text)
import qualified Data.Text           as Text
import           Data.Text.Binary    ()
import           Data.Typeable

-- ------------------------------------------------------------
-- {- the new DocDesc with typed fields

type Field = Text

type FieldRank = Int

data FieldValue = FV_Int !Int
                | FV_Float !Float
                | FV_Text !Text
                | FV_Binary !ByteString
                | FV_Null
                deriving (Eq, Show)

instance IsString FieldValue where
  fromString = FV_Text . fromString

instance NFData FieldValue where
  rnf fv = fv `seq` ()

instance FromJSON FieldValue where
  parseJSON v = case v of
    String s -> pure $ toFieldValue s
    Number n -> pure $ case Scientific.floatingOrInteger n of
                         Left f  -> toFieldValue (f :: Float)
                         Right i -> toFieldValue (i :: Int)
    _ -> mzero

instance ToJSON FieldValue where
  toJSON fv = case fv of
    FV_Int i    -> toJSON i
    FV_Float f  -> toJSON f
    FV_Text s   -> toJSON s
    FV_Binary _ -> Null -- TODO: maybe base64 encode binary values
    FV_Null     -> Null

class ToFieldValue a where
  toFieldValue :: a -> FieldValue

instance ToFieldValue Int where
  toFieldValue = FV_Int

instance ToFieldValue Float where
  toFieldValue = FV_Float

instance ToFieldValue Text where
  toFieldValue = FV_Text

instance a ~ Char => ToFieldValue [a] where
  toFieldValue = toFieldValue . Text.pack

instance ToFieldValue a => ToFieldValue (Maybe a) where
  toFieldValue (Just a) = toFieldValue a
  toFieldValue Nothing  = FV_Null

class FromFieldValue a where
  fromFieldValue :: FieldValue -> Maybe a

instance FromFieldValue Text where
  fromFieldValue (FV_Text s) = Just s
  fromFieldValue _           = Nothing

newtype DocDesc
  = DocDesc { unDesc :: HM.HashMap Field FieldValue }
  deriving (Eq, Show, NFData, Typeable)

-- | Smart constructor for document descriptions.

mkDocDesc :: HM.HashMap Field FieldValue -> DocDesc
mkDocDesc o
    = DocDesc $!! o

instance Binary DocDesc where
    put = put . encode . unDesc
    get = do bs <- get
             case decode bs of
               Nothing -> fail "DocDesc.get: error in decoding from JSON"
               Just x  -> return $! (mkDocDesc x)

instance ToJSON DocDesc where
  toJSON = toJSON . unDesc

instance FromJSON DocDesc where
  parseJSON v = mkDocDesc <$> parseJSON v

-- | The empty description.

empty :: DocDesc
empty = mkDocDesc HM.empty

size :: DocDesc -> Int
size = HM.size . unDesc

fields :: DocDesc -> [Field]
fields = HM.keys . unDesc

-- | Check if document description is empty.

null :: DocDesc -> Bool
null (DocDesc m) = HM.null m

-- | Insert key value pair into description.

insert :: ToFieldValue v => Field -> v -> DocDesc -> DocDesc
insert k v (DocDesc m)
    = mkDocDesc $ HM.insert k (toFieldValue v) m

-- | Remove a key value pair

delete :: Field -> DocDesc -> DocDesc
delete k (DocDesc m)
    = mkDocDesc $ HM.delete k m

-- | Union of two descriptions.

union :: DocDesc -> DocDesc -> DocDesc
union (DocDesc m1) (DocDesc m2)
    = mkDocDesc $ HM.union m1 m2

-- | restrict a DocDesc map to a set of fields

restrict :: [Field] -> DocDesc -> DocDesc
restrict ks (DocDesc m)
    = mkDocDesc $ HM.filterWithKey sel m
      where
        sel k _v = k `elem` ks

deleteNull :: DocDesc -> DocDesc
deleteNull (DocDesc m)
    = mkDocDesc $ HM.filter notNull m
      where
        notNull FV_Null = False
        notNull _       = True

lookupValue :: Field -> DocDesc -> FieldValue
lookupValue k (DocDesc m)
  = HM.lookupDefault FV_Null k m

lookup :: FromFieldValue v => Field -> DocDesc -> Maybe v
lookup k d = fromFieldValue . lookupValue k $ d

lookupText :: Field -> DocDesc -> Text
lookupText k d
    = fromMaybe "" . lookup k $ d

-- | Create a document description from as list

fromList :: ToFieldValue v => [(Field, v)] -> DocDesc
fromList l = mkDocDesc $ HM.fromList (map (second toFieldValue) l)

-- | Create a list from a document description.

toList :: DocDesc -> [(Text, FieldValue)]
toList (DocDesc m) = HM.toList m

-- ------------------------------------------------------------
