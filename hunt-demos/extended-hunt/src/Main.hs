{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
module Main where

import           Data.IntMap.Strict            (IntMap)
import qualified Data.IntMap.Strict            as IM
import           Control.DeepSeq
import           Data.Default
import           Data.Binary                   hiding (Word)
import           Data.Typeable
import           Data.Maybe
import qualified Data.Map                      as M
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Bijection

import           Hunt.Common
import qualified Hunt.Common.DocIdMap          as DM
import           Hunt.Index.Index
import qualified Hunt.Index.Index              as Ix
import           Hunt.Index.Proxy.KeyIndex
import           Hunt.Interpreter.Interpreter
import qualified Hunt.Interpreter.Interpreter  as H
import           Hunt.Interpreter.Command
import           Hunt.Index.Schema.Normalize.Int
import           Hunt.Index.IndexImpl
import           Hunt.Query.Language.Grammar
import           System.Environment

main :: IO ()
main = do
  hunt <- (initHunt :: IO DefHuntEnv)
          >>= \e -> return e { huntTypes = cRealInt:huntTypes e}

  runCmd hunt $ InsertContext "number" def { cxType = cRealInt }

  runCmd hunt $ Insert ApiDocument
              { apiDocUri      = "id://1"
              , apiDocIndexMap = M.fromList [("number", "index only 3 numbers 44")]
              , apiDocDescrMap = M.empty
              }

  res <- runCmd hunt $ Search (QContext ["number"] (QWord QNoCase "3")) 0 10
  putStrLn . show $ res

  res <- runCmd hunt $ Search (QContext ["number"] (QWord QNoCase "23")) 0 10
  putStrLn . show $ res

  res <- runCmd hunt $ Search (QContext ["number"] (QRange "3" "44")) 0 10
  putStrLn . show $ res

  res <- runCmd hunt $ Search (QContext ["number"] (QRange "6" "41")) 0 10
  putStrLn . show $ res

  return ()


cRealInt :: ContextType
cRealInt = CType
  { ctName     = "realInt"
  , ctRegEx    = "([-]?[0-9]*)"
  , ctValidate = CValidator $ isInt
  , ctIxImpl   = mkIndex (Ix.empty :: IntIndex Occurrences)
  }

instance Bijection Int Text where
  from = getInt
  to = T.pack . show

newtype IntIndex _v
  = IntIx { intIx :: KeyProxyIndex Text IntMap Occurrences }
  deriving (Eq, Show, NFData, Typeable)

mkIntIx :: KeyProxyIndex Text IntMap Occurrences
        -> IntIndex _v
mkIntIx x = IntIx $! x

-- ----------------------------------------------------------------------------

instance Binary (IntIndex v) where
  put = put . intIx
  get = get >>= return . mkIntIx

-- ----------------------------------------------------------------------------

instance Index IntIndex where
  type IKey IntIndex v = Text
  type IVal IntIndex v = Occurrences

  insertList wos (IntIx i)
    = mkIntIx $ insertList wos i

  deleteDocs docIds (IntIx i)
    = mkIntIx $ deleteDocs docIds i

  empty
    = mkIntIx $ empty

  fromList l
    = mkIntIx $ fromList l

  toList (IntIx i)
    = toList i

  search t k (IntIx i)
    = search t k i

  lookupRange k1 k2 (IntIx i)
    = lookupRange k1 k2 i

  unionWith op (IntIx i1) (IntIx i2)
    = mkIntIx $ unionWith op i1 i2

  unionWithConv
    = error "IntIndex unionWithConv: cannot be used there because type variable v is fixed"

  map f (IntIx i)
    = mkIntIx $ Ix.map f i

  mapMaybe f (IntIx i)
    = mkIntIx $ Ix.mapMaybe f i

  keys (IntIx i)
    = keys i



-- implementing the index typeclass for general intmap index
--
instance Index IntMap where
  type IKey IntMap v = Int
  type IVal IntMap v = Occurrences
  type ICon IntMap v = (NFData v, Occurrences ~ v)

  insertList kos i
    = IM.union i (IM.fromList kos)

  deleteDocs ks i
    = IM.mapMaybe (\m -> let dm = DM.diffWithSet m ks
                         in if DM.null dm then Nothing else Just dm) i

  empty
    = IM.empty

  fromList l
    = IM.fromList l

  toList i
    = IM.toList i

  search _ k i
    = case IM.lookup k i of
        (Just res) -> [(k, res)]
        _          -> []

  lookupRange k1 k2 i
    = IM.toList $ IM.filterWithKey (\k _ -> k >= k1 &&k <= k2) i

  unionWith f i1 i2 
    = IM.unionWith f i1 i2

  unionWithConv
    = error "not yet implemented"

  map f i
    = IM.map f i

  mapMaybe f i
    = IM.mapMaybe f i

  keys i
    = IM.keys i
