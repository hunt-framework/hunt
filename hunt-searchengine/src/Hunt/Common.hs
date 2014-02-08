{- |
  Module     : Hunt.Index.Common
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

module Hunt.Common
  (
    module Hunt.Common.BasicTypes
  , module Hunt.Common.Occurrences.Compression
  , module Hunt.Common.DocId
  , module Hunt.Common.DocIdMap
  , module Hunt.Common.Document
  , module Hunt.Common.Occurrences
  , module Hunt.Common.Positions
  , module Hunt.Common.RawResult
  , module Hunt.Common.ApiDocument
  , module Hunt.Index.Schema
  )
where

import           Hunt.Common.ApiDocument             (ApiDocument (..))
import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import           Hunt.Common.DocIdMap                (DocIdMap, DocIdSet)
import           Hunt.Common.Document                (Document (..))
import           Hunt.Common.Occurrences             (Occurrences)
import           Hunt.Common.Occurrences.Compression
import           Hunt.Common.Positions               (Positions)
import           Hunt.Common.RawResult
import           Hunt.Index.Schema
