module Holumbus.Indexer.Indexer where

import Holumbus.Index.Proxy.ContextIndex

-- | Generic compbination of Index and DocTable
type Indexer i v dt = (i v, dt)

-- | Generic combination of Index and DocTable using Contexts
type ContextIndexer i v dt = (ContextIndex i v, dt)

