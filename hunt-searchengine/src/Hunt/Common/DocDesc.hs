{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable         #-}
module Hunt.Common.DocDesc where


import           Control.Applicative  ((<$>))
import           Control.DeepSeq

import           Data.Aeson
import           Data.Binary
import           Data.Text.Binary     ()
import           Data.Text            (Text)
import           Data.Typeable
import           Data.Map.Strict      (Map)
import qualified Data.Map.Strict      as SM

newtype DocDesc v
  = DocDesc { unDesc :: Map Text v }
  deriving (Eq, Show, NFData, Typeable, Ord)

instance Binary v => Binary (DocDesc v) where
  put = put . unDesc
  get = get >>= return . DocDesc

instance ToJSON v => ToJSON (DocDesc v) where
  toJSON (DocDesc v) = toJSON v

instance FromJSON v => FromJSON (DocDesc v) where
  parseJSON o = DocDesc <$> parseJSON o

-- | The empty description.
empty :: DocDesc v
empty = DocDesc $ SM.empty

-- | Insert key value pair into description.
insert :: Text -> v -> DocDesc v -> DocDesc v
insert k v (DocDesc m) = DocDesc $! SM.insert k v m

-- | Check if document description is empty.
null :: DocDesc v -> Bool
null (DocDesc m) = SM.null m

-- | Union of two descriptions.
union :: DocDesc v -> DocDesc v -> DocDesc v
union (DocDesc m1) (DocDesc m2) = DocDesc $! SM.union m1 m2

-- | Create a document description from as list
fromList :: [(Text,v)] -> DocDesc v
fromList l = DocDesc $! SM.fromList l

-- | Create a list from a document description.
toList :: DocDesc v -> [(Text, v)]
toList (DocDesc m) = SM.toList m
