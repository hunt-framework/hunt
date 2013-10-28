{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- ----------------------------------------------------------------------------

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

module Holumbus.Index.Common
  (
    module Holumbus.Common.BasicTypes
  , module Holumbus.Common.Document
  , module Holumbus.Common.DocId
  , module Holumbus.Common.DocIdMap
  , module Holumbus.Common.Occurrences
  , module Holumbus.Common.RawResult
  )
where

import           Holumbus.Common.BasicTypes
import           Holumbus.Common.DocId
import           Holumbus.Common.DocIdMap    (DocIdMap, DocIdSet)
import           Holumbus.Common.Document    (Description, Document (..),
                                              DocumentWrapper (wrap, unwrap))
import           Holumbus.Common.Occurrences (Occurrences, Positions)
import           Holumbus.Common.RawResult

-- ------------------------------------------------------------
