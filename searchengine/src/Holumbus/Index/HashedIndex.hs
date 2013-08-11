{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Index.HashedIndex
  ( Document
  , Documents
  , SmallDocuments
  , emptySmallDocuments

  , Inverted
  , emptyInverted
  , removeDocIdsInverted

  , CompactInverted
  , emptyCompactInverted
  , inverted2compactInverted
  
  , emptyDocuments
  )
where

import           Holumbus.Index.Common                            (Document (..),
                                                                   Occurrences,
                                                                   fromList,
                                                                   toList)

import           Holumbus.Index.HashedDocuments                   (Documents (..), emptyDocuments)

import qualified Holumbus.Index.Text.Inverted.CompressedPrefixMem as PM


-- ------------------------------------------------------------

{- .1: direct use of prefix tree with simple-9 encoded occurences

   concerning efficiency this implementation is about the same as the 2. one,
   space and time are minimally better, the reason could be less code working with classes

import           Holumbus.Index.Text.Inverted.PrefixMem

-- -}
-- ------------------------------------------------------------

{- .2: indirect use of prefix tree with simple-9 encoded occurences via InvertedCompressed

   minimal overhead compared to .1
   but less efficient in time (1598s / 1038s) and space
   total mem use (2612MB / 2498MB) than .3

import qualified Holumbus.Index.Inverted.CompressedPrefixMem    as PM

type Inverted                   = PM.InvertedCompressed

emptyInverted                   :: Inverted
emptyInverted                   = PM.emptyInvertedCompressed

-- -}

-- ------------------------------------------------------------
-- {- remove "--" to coment out region

{- .3: indirect prefix tree without compression of position sets

   best of these 3 implementations

   implementations with serializations become much more inefficient
   in runtime and are not worth to be considered
-}


type Inverted                   = PM.Inverted0

emptyInverted                   :: Inverted
emptyInverted                   = PM.emptyInverted0

removeDocIdsInverted            :: Occurrences -> Inverted -> Inverted
removeDocIdsInverted            = PM.removeDocIdsInverted

type CompactInverted            = PM.InvertedOSerialized

emptyCompactInverted            :: CompactInverted
emptyCompactInverted            = PM.emptyInvertedOSerialized

inverted2compactInverted        :: Inverted -> CompactInverted
inverted2compactInverted        = fromList PM.emptyInvertedOSerialized . toList

type SmallDocuments             = Documents

emptySmallDocuments             :: SmallDocuments
emptySmallDocuments             = emptyDocuments
