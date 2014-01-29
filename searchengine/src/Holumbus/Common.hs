{- |
  Module     : Holumbus.Index.Common
  Copyright  : Copyright (C) 2007-2012 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  Common data types shared by all index types and a unified interface for
  all different index types. This module defines the common interfaces of
  indexes and their document tables as well as full-text caches.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Common
  (
    module Holumbus.Common.BasicTypes
  , module Holumbus.Common.Occurrences.Compression
  , module Holumbus.Common.DocId
  , module Holumbus.Common.DocIdMap
  , module Holumbus.Common.Document
  , module Holumbus.Common.Occurrences
  , module Holumbus.Common.Positions
  , module Holumbus.Common.RawResult
  , module Holumbus.Common.ApiDocument
  , module Holumbus.Index.Schema
  )
where

import           Holumbus.Common.ApiDocument  (ApiDocument (..))
import           Holumbus.Common.BasicTypes
import           Holumbus.Common.Occurrences.Compression
import           Holumbus.Common.DocId
import           Holumbus.Common.DocIdMap     (DocIdMap, DocIdSet)
import           Holumbus.Common.Document     (Document (..))
import           Holumbus.Common.Occurrences  (Occurrences)
import           Holumbus.Common.Positions    (Positions)
import           Holumbus.Common.RawResult
import           Holumbus.Index.Schema
