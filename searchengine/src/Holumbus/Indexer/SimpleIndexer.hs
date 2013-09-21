module Holumbus.Indexer.SimpleIndexer where

import           Control.DeepSeq

import           Data.Binary                  (Binary (..))

import           Holumbus.Index.Common

import           Holumbus.Index.TextIndex     (TextIndex)

import           Holumbus.DocTable.DocTable   (DocTable)

import           Holumbus.Indexer.Indexer
import qualified Holumbus.Indexer.TextIndexer as TIxx

-- ----------------------------------------------------------------------------

-- TODO: constraints for ii and di?
data SimpleIndexer ii di = SimpleIndexer
  { siIndex    :: ii
  , siDocTable :: di
  }

-- ----------------------------------------------------------------------------

instance (NFData ii, NFData di) => NFData (SimpleIndexer ii di) where
  rnf (SimpleIndexer ii di) = rnf ii `seq` rnf di

instance (Binary ii, Binary di) => Binary (SimpleIndexer ii di) where
  get = do
    ii <- get
    di <- get
    return $ SimpleIndexer ii di
  put (SimpleIndexer ii di) = put ii >> put di

-- ----------------------------------------------------------------------------

instance (TextIndex ii, DocTable di) =>
         Indexer (SimpleIndexer ii di) where
  type IxIndex    (SimpleIndexer ii di) = ii
  type IxDocTable (SimpleIndexer ii di) = di
  type IxElem     (SimpleIndexer ii di) = Words -- FIXME: nonono
  modIndex    i ix   = ix{siIndex    = i}
  modDocTable d ix   = ix{siDocTable = d}
  ixIndex            = siIndex
  ixDocTable         = siDocTable
  insert             = TIxx.insert
  update             = TIxx.update
  modify             = TIxx.modify
