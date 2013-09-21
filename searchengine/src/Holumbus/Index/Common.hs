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
    module Holumbus.Index.Common.BasicTypes
  , module Holumbus.Index.Common.Document
  , module Holumbus.Index.Common.DocId
  , module Holumbus.Index.Common.DocIdMap
  , module Holumbus.Index.Common.Occurrences
  , module Holumbus.Index.Common.RawResult
  )
where

import           Holumbus.Index.Common.BasicTypes
import           Holumbus.Index.Common.DocId
import           Holumbus.Index.Common.DocIdMap    (DocIdMap)
import           Holumbus.Index.Common.Document    (Description, Document (..),
                                                    DocumentWrapper (wrap, unwrap))
import           Holumbus.Index.Common.Occurrences (Occurrences, Positions)
import           Holumbus.Index.Common.RawResult

-- ------------------------------------------------------------