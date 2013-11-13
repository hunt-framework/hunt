module Holumbus.Common.Schema where

import Holumbus.Common.BasicTypes
import Data.Aeson
import Data.Text
import Data.Map
import Data.Binary
import Control.Monad                   (mzero)

-- | Schema 
type ContextSchema = Map Context ContextType

type ContextType = (CType, CRegex, [CNormalizer])

type CRegex = Text
data CType = CText | CInt
  deriving (Show, Eq)
 
data CNormalizer = CUpperCase | CLowerCase
  deriving (Show, Eq)


-- | json instances
instance FromJSON CType where
  parseJSON (String s) 
    = case s of
        "ctext" -> return CText
        "cint"  -> return CInt
        _       -> mzero
  parseJSON _ = mzero

instance ToJSON CType where
  toJSON o = case o of
    CText -> "ctext"
    CInt  -> "cint"

instance FromJSON CNormalizer where
  parseJSON (String s) 
    = case s of
        "uppercase" -> return CUpperCase
        "lowercase" -> return CLowerCase
        _           -> mzero
  parseJSON _ = mzero

instance ToJSON CNormalizer where
  toJSON o = case o of
    CUpperCase -> "uppercase"
    CLowerCase -> "lowercase"

-- | binary instances
instance Binary CType where
  put (CText) = put (0 :: Word8)
  put (CInt)  = put (1 :: Word8)
  
  get = do
    t <- get :: Get Word8
    case t of 
      0 -> return CText
      1 -> return CInt

instance Binary CNormalizer where
  put (CUpperCase) = put (0 :: Word8)
  put (CLowerCase) = put (1 :: Word8)
  
  get = do
    t <- get :: Get Word8
    case t of 
      0 -> return CUpperCase
      1 -> return CLowerCase

