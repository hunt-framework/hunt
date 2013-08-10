{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Holumbus.Index.Common2 where

import           Control.Arrow             (first)
import           Control.Monad.State

import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.IntMap               (IntMap)
import qualified Data.IntMap               as IM

import           Holumbus.Index.Common     hiding (Occurrences, RawResult, HolIndex, Positions, DocId)
import qualified Holumbus.Index.Common     as Co
import qualified Holumbus.Data.PrefixTree  as PT 

import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as BS

import           Holumbus.Data.PrefixTree  (PrefixTree)
import           Data.EnumSet              (EnumSet)
import           Data.Maybe

{- |
  Module     : Holumbus.Index.Common

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
  -- | internal represenation of document id
  type DocId     i :: *
  -- | internal representation of info the index hold for a document
  type DocInfo   i :: *
  -- | search key for the index. f.e.: text for text index
  type SearchKey i :: *
 
  -- | insert key (word) with generic docinfo (occurrences) into context
  insert :: Context -> SearchKey i -> DocInfo i -> i -> i 
  -- | update DocInfo for key (word) in context ..??
  update :: Context -> SearchKey i -> (DocId i -> DocInfo i) -> i -> i 
  -- | remove document from all contexts
  delete :: DocId i -> i -> i



-- | example1 simple index storing dates serialized as number
--   associated with URI
type Date = Int
data SampleIndex = SampleIndex { six :: IntMap [URI] }

instance Index SampleIndex where
  type DocId     SampleIndex = URI
  type DocInfo   SampleIndex = [URI]
  type SearchKey SampleIndex = Date

  insert c d u i = SampleIndex $ IM.insert d u $ (six i) 
  update c d f i = undefined
  delete d i     = undefined

-- | example2 - holumbus index like its currently defined
type Occurrences        = Map Co.DocId Positions
type Positions          = EnumSet Position
data Inverted = Inverted { iix :: Map Context (PrefixTree Occurrences) }

instance Index Inverted where
  type DocId     Inverted = Co.DocId
  type DocInfo   Inverted = Occurrences
  type SearchKey Inverted = Word

  insert = undefined
  update = undefined
  delete = undefined

{--
 - query api
 -
 - since different index implementations most likely require
 - diffrent search operations, we should be able to define
 - them seperate from the general typeclass
 -
 - why does the tree return lists not maps?
 -}

data Query i = Query {
  process :: (Index i) => i -> Context -> SearchKey i -> Map (SearchKey i) (DocInfo i)
}

-- | example1 - queries for the date index
exactMatch :: Query SampleIndex
exactMatch = Query {
  process = \i c d -> M.singleton d $ concat $ maybeToList $ IM.lookup d $ six i
}

-- | example2 - query for current holumbus index
prefixCaseQuery :: Query Inverted
prefixCaseQuery = Query {
  process = \i c d -> M.fromList $ fmap (first T.pack) $ PT.prefixFindWithKeyBF (T.unpack d) $ getContext c i
}
getContext c = fromMaybe PT.empty . M.lookup c . iix


{--
 - combining documents and index:
 -
 - module Holumbus.Index.DocTable.Memory

 -
 - or ... not using typeclasses at all ?? just working on
 - two arbitary datastructures put in a state?
 -}


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
