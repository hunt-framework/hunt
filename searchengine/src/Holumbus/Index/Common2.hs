{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Holumbus.Index.Common2 where

import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Data.Map                  (Map)
import qualified Data.Map                  as M

import           Holumbus.Index.Common     hiding (Occurrences)
import qualified Holumbus.Data.PrefixTree  as PT 


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
 -
 - having the occurrences defined by a typeclass 
 - makes sense as well. if our index implementation
 - uses a different data structure its likely the
 - occurrence will as well.
 -}

class (IndexData o) => Index i o where
  insert :: Context -> Word -> o -> i -> i 
  delete :: DocId -> i -> i
  update :: Context -> Word -> o -> i -> i


-- | example
newtype Occurrences = DocIdMap Positions
newtype Inverted = Inverted { invIndex :: M.Map Context (PT.PrefixTree Occurrences) }

instance Index Inverted Occurrences where
  insert c w o i = undefined
  delete id i    = undefined
  update c w o i = undefined


{-
 - the occurrences in the current implementation should be 
 - defined by a typeclass. this allows us to exchange the
 - implementation to try out different compressions for example
 -
 - also it allows us to change the representation completlty if
 - our underlying index structure changes  
 -}

class IndexData x where
  toIndexData   :: x
  fromIndexData :: x
  -- | @TODO: what functions would we need here??

instance IndexData Occurrences where
  toIndexData = undefined
  fromIndexData = undefined
 



{-
 - all kind of different search operations moved out
 - of the index typeclass into this datatype
 -
 - the available operations depend strongly on the
 - underlying datastructure - so it makes sense to
 - have the possibility to implements differnt kinds
 - of operations for different types of indexes
 -
 - maybe someone implements some index that isn't able
 - to distinguish between upper and lower cased 
 - words. the old typeclass would anyhow force him to 
 - implement those operations 
 -}
data Query i = Query {
  process :: i -> Context -> Text -> RawResult
}

prefixQuery :: Query Inverted
prefixQuery = Query (\i c t -> undefined)

prefixCaseQuery :: Query Inverted
prefixCaseQuery = Query (\i c t -> undefined)

lookupQuery :: Query Inverted
lookupQuery = Query (\i c t -> undefined)

lookupCaseQuery :: Query Inverted
lookupCaseQuery = Query (\i c t -> undefined)

{--
 - HolDocuments typeclass currently is a little big
 - and messy as well. maybe we find a way to redruce
 - complexity there.
 - 
 - I'm not so sure about this typeclass at all.
 - how much flexibility do we need here? 
 - i'm not sure how this makes sense right now
 -
 - our current doctable is a in memory map
 - whatelse implementations might be possible?
 - file storage? database? list?
 -}


class DocumentTable d where
  insert_ :: d
  update_ :: d
  delete_ :: d
  lookup  :: DocId -> Document
   
newtype DocMap = DocMap { idToDoc :: DocIdMap ByteString }

instance Document DocMap where
  insert_ = undefined
  update_ = undefined
  delete_ = undefined


{--
 - we thought about some kind of decorator helper to
 - be able to easily inject extra features into the 
 - index, without having to change the index itself
 -
 - we should be able to use this structure for ...
 - ... caching results
 - ... having different kind of compression
 - ... maybe being able to have multiple layers to
 -     implement efficient deletes with tmp tables
 -
 - default implementation for each operation should
 - be => do nothing. so each interceptor only needs
 - to implement the operation it actually needs
 -}

class Interceptor x where 
  -- | do something before operation
  beforeInsert :: x
  beforeUpdate :: x
  beforeDelete :: x
  beforeQuery  :: x
  
  -- | do something after operation
  afterInsert  :: x
  afterUpdate  :: x
  afterDelete  :: x
  afterQuery   :: x


{--
 - to actually being able to use this framework we need some kind
 - of type or typeclass that combines the various index, document 
 - and interceptor implementations
 -}

data Indexer i = Indexer {
}





