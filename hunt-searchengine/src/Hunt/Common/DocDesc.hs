{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hunt.Common.DocDesc where

import           Prelude             hiding (lookup)

import           Control.Arrow       (second)
import           Control.DeepSeq
import           Control.Monad       (mzero)

import           Data.Aeson          (FromJSON (..), Object, Result (..),
                                      ToJSON (..), Value (..), decode, encode,
                                      fromJSON)
import           Data.Binary         (Binary (..))
import qualified Data.HashMap.Strict as HM
import           Data.Maybe          (fromMaybe)
import           Data.Text           (Text)
import           Data.Text.Binary    ()
import           Data.Typeable

-- ------------------------------------------------------------
-- {- the new DocDesc with JSON values as attributes

newtype DocDesc
  = DocDesc { unDesc :: Object }
  deriving (Eq, Show, NFData, Typeable)

instance Binary DocDesc where
    put = put . encode . unDesc
    get = do bs <- get
             case decode bs of
               Nothing -> fail "DocDesc.get: error in decoding from JSON"
               Just x  -> return (DocDesc x)

instance ToJSON DocDesc where
    toJSON = Object . unDesc

instance FromJSON DocDesc where
    parseJSON (Object o) = return (DocDesc o)
    parseJSON _          = mzero

-- | The empty description.

empty :: DocDesc
empty = DocDesc $! HM.empty

-- | Check if document description is empty.

null :: DocDesc -> Bool
null (DocDesc m) = HM.null m

-- | Insert key value pair into description.

insert :: ToJSON v => Text -> v -> DocDesc -> DocDesc
insert k v (DocDesc m)
    = DocDesc $! HM.insert k (toJSON v) m

-- | Remove a key value pair

delete :: Text -> DocDesc -> DocDesc
delete k (DocDesc m)
    = DocDesc $! HM.delete k m

-- | Union of two descriptions.

union :: DocDesc -> DocDesc -> DocDesc
union (DocDesc m1) (DocDesc m2)
    = DocDesc $! HM.union m1 m2

-- | restrict a DocDesc map to a set of fields

restrict :: [Text] -> DocDesc -> DocDesc
restrict ks (DocDesc m)
    = DocDesc $! HM.filterWithKey sel m
      where
        sel k _v = k `elem` ks

lookupValue :: Text -> DocDesc -> Value
lookupValue k (DocDesc m)
    = fromMaybe Null $ HM.lookup k m

lookup :: FromJSON v => Text -> DocDesc -> Maybe v
lookup k d
    = toMaybe . fromJSON . lookupValue k $ d
    where
      toMaybe (Success v) = Just v
      toMaybe _              = Nothing

lookupText :: Text -> DocDesc -> Text
lookupText k d
    = fromMaybe "" . lookup k $ d

-- | Create a document description from as list

fromList :: ToJSON v => [(Text, v)] -> DocDesc
fromList l = DocDesc $!! HM.fromList (map (second toJSON) l)

-- | Create a list from a document description.

toList :: DocDesc -> [(Text, Value)]
toList (DocDesc m) = HM.toList m

-- ------------------------------------------------------------
