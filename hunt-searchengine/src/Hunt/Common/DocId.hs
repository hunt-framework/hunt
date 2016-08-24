{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Hunt.Index.Common.DocId
  Copyright  : Copyright (C) 2014 Uwe Schmidt
  License    : MIT

  Maintainer : Uwe Schmidt (uwe@fh-wedel.de)

  The document identifier type DocId and the newtype DocId' with Show and ToJSOn instances
-}

-- ----------------------------------------------------------------------------

module Hunt.Common.DocId
where

import           Data.Aeson
import           Data.Binary                (Binary (..))
import qualified Data.Binary                as B
import           Data.Digest.Murmur64
import           Data.Text                  (Text)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Builder     as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import qualified Data.Text.Read             as Text

-- ------------------------------------------------------------
{-
-- | The unique identifier of a document.
type DocId = Int

-- ------------------------------------------------------------

-- | Create the null-identifier.
mkNull :: DocId
mkNull = 0

-- | Create the first identifier.
mkFirst :: DocId
mkFirst = 1

-- | Create a 'DocId' from an 'Integer'.
fromInteger :: Integer -> DocId
fromInteger = fromIntegral

-- ------------------------------------------------------------

{-# INLINE mkNull #-}
{-# INLINE mkFirst #-}
{-# INLINE fromInteger #-}

-- -}
-- ------------------------------------------------------------

-- the wrapped DocId
-- currently only used for JSON debug output

newtype DocId = DocId {unDocId :: Int}
    deriving (Eq, Ord)

instance Show DocId where
    show = Text.unpack . toHex . unDocId

instance Binary DocId where
    put = put . unDocId
    get = DocId <$> get

instance ToJSON DocId where
    toJSON (DocId i) = toJSON $ toHex i

mkDocId :: Binary a => a -> DocId
mkDocId = DocId . fromIntegral . asWord64 . hash64 . B.encode

toHex :: Int -> Text
toHex =
  LText.toStrict
  . Builder.toLazyTextWith 18
  . mappend "0x"
  . Builder.hexadecimal

fromHex :: Text -> Maybe Int
fromHex t
  | Right (i, "") <- Text.hexadecimal t = Just i
  | otherwise = Nothing

-- ------------------------------------------------------------
