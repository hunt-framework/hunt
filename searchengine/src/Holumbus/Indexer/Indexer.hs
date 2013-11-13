module Holumbus.Indexer.Indexer where

import Holumbus.Common
import Holumbus.Index.Proxy.ContextIndex

import Data.Text (Text)
import Data.Map  (Map)

import Data.Binary

type Schema = (CType, CRegex, [CNormalizer])

type CRegex = Text
data CType = CText | CInt
  deriving (Show, Eq)

instance Binary CType where
  put (CText) = put (0 :: Word8)
  put (CInt)  = put (1 :: Word8)
  
  get = do
    t <- get :: Get Word8
    case t of 
      0 -> return CText
      1 -> return CInt
      _ -> error "unparsable binary format: CType"

data CNormalizer = CUpperCase | CLowerCase
  deriving (Show, Eq)


instance Binary CNormalizer where
  put (CUpperCase) = put (0 :: Word8)
  put (CLowerCase) = put (1 :: Word8)
  
  get = do
    t <- get :: Get Word8
    case t of 
      0 -> return CUpperCase
      1 -> return CLowerCase
      _ -> error "unparseable binary format: CNormalizer"

type ContextSchema = Map Context Schema

-- | Generic compbination of Index and DocTable
type Indexer i v dt = (i v, dt)

-- | Generic combination of Index and DocTable using Contexts
type ContextIndexer i v dt = (ContextIndex i v, dt, ContextSchema)

