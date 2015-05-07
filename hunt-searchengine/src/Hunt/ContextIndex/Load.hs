module Hunt.ContextIndex.Load where


import           Hunt.Common.BasicTypes
import           Hunt.ContextIndex.Types
import           Hunt.DocTable
import qualified Hunt.Index.IndexImpl as Ix
import           Hunt.Index.Schema

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binary
import           Data.Binary.Get (Get)
import qualified Data.Binary.Get as Get
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.List as List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           System.IO.Error

getContextMap :: ContextTypes -> Get ContextMap
getContextMap cxTypes
  = do m <- Ix.gets' impls :: Get [(Context, Ix.IndexImpl)]
       return (mkContextMap (Map.fromDistinctAscList m))
  where
    impls = fmap ctIxImpl cxTypes

putContextMap :: ContextMap -> Put
putContextMap = put . cxMap

getSchema :: [CNormalizer] -> ContextTypes -> Get Schema
getSchema cxNorm cxTypes
  = do schema <- get :: Get [(Context, ContextSchema)]
       fixed  <- mapM populate schema
       return (Map.fromList fixed)
  where
    populateNormalizer n
      = maybe mzero return (List.find (\m -> cnName m == cnName n) cxNorm)

    populateContextType cxt
      = maybe mzero return (List.find (\t -> ctName t == ctName cxt) cxTypes)

    populate (cx, cxSchema)
      = do nx <- mapM populateNormalizer (cxNormalizer cxSchema)
           ct <- populateContextType (cxType cxSchema)
           return (cx, cxSchema { cxNormalizer = nx
                                , cxType       = ct
                                })

