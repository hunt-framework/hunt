{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Hunt.Index.Common2 where

import           Prelude                   hiding (lookup)
import           Control.Arrow             (first)
import           Control.Monad.State

import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.IntMap               (IntMap)
import qualified Data.IntMap               as IM

import           Hunt.Index.Common     hiding (Occurrences, RawResult, HolIndex, Positions, DocId, insert, lookup)
import qualified Hunt.Index.Common     as CO
import qualified Data.StringMap            as SM

import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as BS

import           Data.EnumSet              (EnumSet)
import           Data.Maybe

{- |
  Module     : Hunt.Index.Common

  this module contains ideas and prototypes for a
  possible refactoring for  index / documents
  data structure.
-}

{--
 - the index typeclass should be more lightweight
 - 
 - in fact: insert,update and delete should do it.
 - (a single and a batch processing version each)
 - 
 - it would make sense to having the internal DocId
 - transparent to the api users. they should have
 - to deal with URI's only. Not sure how to
 - achive that though.
 -}
class Index i where
  -- | Internal representation of Key
  type IKey      i :: *
  -- | Representation of data within Index
  type IValue    i :: *
  -- | Result type for lookup operations
  type IResult   i :: *
  -- | Search Operations associated with this index implementation 
  data ISearchOp i :: *
   
  -- | Insert Key (f.e.: word) with DocInfo (f.e.: Occurrences) for a context
  insert :: Context -> IKey i -> IValue i -> i -> i 
  -- | Remove DocInfo for DocId from all Contexts
  delete :: IKey i -> i -> i
  -- | Lookup for DocInfo in Index
  lookup :: Context -> ISearchOp i -> i -> IResult i


type MyMap = Map Int String

-- | Simple example for Map ignoring Contexts
instance Index MyMap where
  type IKey MyMap      = Int
  type IValue MyMap    = String
  type IResult MyMap   = Maybe String
  data ISearchOp MyMap = SimpleLookup (IKey MyMap)

  insert _ k v ix              = M.insert k v ix
  delete k ix                  = M.delete k ix
  lookup _ (SimpleLookup k) ix = M.lookup k ix


emptyMyMap :: MyMap
emptyMyMap = M.empty

test :: IO ()
test = do
  let key = 1::Int
  let x1 = insert "" key "eins" emptyMyMap
  let mr = lookup "" (SimpleLookup key) x1
  print mr

-- | example2 - holumbus index like its currently defined
{--
 - type Occurrences        = Map Co.DocId Positions
type Positions          = EnumSet Position
data Inverted = Inverted { iix :: Map Context (SM.StringMap Occurrences) }

instance Index Inverted where
  type DocId     Inverted = Co.DocId
  type DocInfo   Inverted = Occurrences
  type SearchKey Inverted = Word

  insert = undefined
  update = undefined
  delete = undefined
--}

{--
 - combining documents and index:
 -
 - module Hunt.Index.DocTable.Memory

 -
 - or ... not using typeclasses at all ?? just working on
 - two arbitary datastructures put in a state?
 -}

{-
data Index_ = Index_ { ix :: Map Text [Int] }
data Doc = Doc { dx :: Map Int Text }

docs = Doc $ M.insert (1::Int) "document eins" $ M.empty
index = Index_ $ M.fromList [("document",[1::Int]), ("eins",[1::Int])]

emptyIndex = (index,docs)

insertM_ :: State (Index_, Doc) ()
insertM_ = do
  (i,d) <- get
  return ()
  

main :: IO ()
main = print $ evalState insertM_ emptyIndex
--}
