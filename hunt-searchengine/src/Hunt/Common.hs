{- |
  Module     : Hunt.Index.Common
  License    : MIT

  Maintainer : Ulf Sauer
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
  , module Hunt.Common.DocId
  , module Hunt.Common.DocIdMap
  , module Hunt.Common.DocIdSet
  , module Hunt.Common.Document
  , module Hunt.Common.Occurrences
  , module Hunt.Common.Positions
  , module Hunt.Common.RawResult
  , module Hunt.Common.ApiDocument
  , module Hunt.Index.Schema
  )
where

import           Hunt.Common.ApiDocument (ApiDocument (..))
import           Hunt.Common.BasicTypes
import           Hunt.Common.DocId
import           Hunt.Common.DocIdMap    (DocIdMap)
import           Hunt.Common.DocIdSet    (DocIdSet)
import           Hunt.Common.Document    (Document (..))
import           Hunt.Common.Occurrences (Occurrences)
import           Hunt.Common.Positions   (Positions)
import           Hunt.Common.RawResult
import           Hunt.Index.Schema
