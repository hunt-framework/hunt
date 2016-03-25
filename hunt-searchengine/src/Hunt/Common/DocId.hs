{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

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

import           Control.Monad (liftM)
import           Data.Aeson
import           Data.Binary (Binary (..))
import qualified Data.Binary as B
import           Data.Digest.Murmur64
import qualified Data.Text                  as TextS
import qualified Data.Text.Lazy             as Text
import qualified Data.Text.Lazy.Builder     as Text
import qualified Data.Text.Lazy.Builder.Int as Text
import qualified Data.Text.Read             as TextS

import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as UV

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
    show = TextS.unpack . toHex . unDocId

instance Binary DocId where
    put = put . unDocId
    get = DocId <$> get

instance ToJSON DocId where
    toJSON (DocId i) = toJSON $ toHex i

mkDocId :: Binary a => a -> DocId
mkDocId = DocId . fromIntegral . asWord64 . hash64 . B.encode

toHex :: Int -> TextS.Text
toHex y = Text.toStrict (Text.toLazyText b)
  where b = "0x" `mappend` Text.hexadecimal y
{-# INLINE toHex #-}

fromHex :: TextS.Text -> Maybe Int
fromHex s =
  case TextS.hexadecimal s of
    Right (x, s') | TextS.null s' -> Just x
    _                             -> Nothing
{-# INLINE fromHex #-}

-- ------------------------------------------------------------

newtype instance UV.MVector s DocId = MV_DocId (UV.MVector s Int)
newtype instance UV.Vector DocId = V_DocId (UV.Vector Int)

instance UV.Unbox DocId


instance GM.MVector UV.MVector DocId where
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicOverlaps #-}
  {-# INLINE basicUnsafeNew #-}
  {-# INLINE basicUnsafeReplicate #-}
  {-# INLINE basicUnsafeRead #-}
  {-# INLINE basicUnsafeWrite #-}
  {-# INLINE basicClear #-}
  {-# INLINE basicSet #-}
  {-# INLINE basicUnsafeCopy #-}
  {-# INLINE basicUnsafeGrow #-}

#if MIN_VERSION_vector(0,11,0)
  basicInitialize (MV_DocId v) = GM.basicInitialize v
#endif

  basicLength (MV_DocId v) = GM.basicLength v
  basicUnsafeSlice i n (MV_DocId v) = MV_DocId $ GM.basicUnsafeSlice i n v
  basicOverlaps (MV_DocId v1) (MV_DocId v2) = GM.basicOverlaps v1 v2
  basicUnsafeNew n = MV_DocId `liftM` GM.basicUnsafeNew n
  basicUnsafeReplicate n (DocId x) = MV_DocId `liftM` GM.basicUnsafeReplicate n x
  basicUnsafeRead (MV_DocId v) i = DocId `liftM` GM.basicUnsafeRead v i
  basicUnsafeWrite (MV_DocId v) i (DocId x) = GM.basicUnsafeWrite v i x
  basicClear (MV_DocId v) = GM.basicClear v
  basicSet (MV_DocId v) (DocId x) = GM.basicSet v x
  basicUnsafeCopy (MV_DocId v1) (MV_DocId v2) = GM.basicUnsafeCopy v1 v2
  basicUnsafeMove (MV_DocId v1) (MV_DocId v2) = GM.basicUnsafeMove v1 v2
  basicUnsafeGrow (MV_DocId v) n = MV_DocId `liftM` GM.basicUnsafeGrow v n

instance GV.Vector UV.Vector DocId where
  {-# INLINE basicUnsafeFreeze #-}
  {-# INLINE basicUnsafeThaw #-}
  {-# INLINE basicLength #-}
  {-# INLINE basicUnsafeSlice #-}
  {-# INLINE basicUnsafeIndexM #-}
  {-# INLINE elemseq #-}
  basicUnsafeFreeze (MV_DocId v) = V_DocId `liftM` GV.basicUnsafeFreeze v
  basicUnsafeThaw (V_DocId v) = MV_DocId `liftM` GV.basicUnsafeThaw v
  basicLength (V_DocId v) = GV.basicLength v
  basicUnsafeSlice i n (V_DocId v) = V_DocId $ GV.basicUnsafeSlice i n v
  basicUnsafeIndexM (V_DocId v) i = DocId `liftM` GV.basicUnsafeIndexM v i
  basicUnsafeCopy (MV_DocId mv) (V_DocId v) = GV.basicUnsafeCopy mv v
  elemseq _ = seq
