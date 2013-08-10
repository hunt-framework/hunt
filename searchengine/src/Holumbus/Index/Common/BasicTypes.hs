{-# OPTIONS #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Index.Common.BasicTypes
  Copyright  : Copyright (C) 2011 Sebastian M. Schlatt, Timo B. Huebel, Uwe Schmidt
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: none portable

  Basic data types for index

-}

-- ----------------------------------------------------------------------------

module Holumbus.Index.Common.BasicTypes
where

import           Data.Word      ( Word32 )
import           Data.Text
import           Data.Map

-- ------------------------------------------------------------

-- | The URI describing the location of the original document.
type URI                        = Text

-- | The title of a document.
type Title                      = Text

-- | The content of a document.
type Content                    = Text

-- | The position of a word in the document.
--type Position                 = Int
type Position                   = Word32

-- | The name of a context.
type Context                    = Text

-- | A single word.
type Word                       = Text

-- | Positions of Words for each context.
type Words        = Map Context WordList

-- | Positions of words in the document.
type WordList     = Map Word [Position]


-- ------------------------------------------------------------
