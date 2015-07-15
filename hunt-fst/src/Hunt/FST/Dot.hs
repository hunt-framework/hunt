{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
module Hunt.FST.Dot where

import           Data.ByteString.Builder
import           Data.ByteString.Lazy (ByteString)
import           Data.Char (chr)
import           Hunt.FST.Arcs (Arc(..), Arcs)
import qualified Hunt.FST.Arcs as Arcs
import           Hunt.FST.Register
import           Hunt.FST.Types
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Monoid

data Register a = Register {
    dotBase     :: !a
  , dotRendered :: !(HashSet (StateRef, Arc))
  , dotOutput   :: !Builder
  }

instance Register1 a => Register1 (Register a) where
  type Output (Register a) = ByteString
  empty             = Hunt.FST.Dot.empty
  output            = Hunt.FST.Dot.output
  replaceOrRegister = Hunt.FST.Dot.replaceOrRegister

empty :: Register1 a =>  Register a
empty = Register {
    dotBase = Hunt.FST.Register.empty
  , dotRendered = mempty
  , dotOutput   = mempty
  }

replaceOrRegister :: Register1 a
                  => UncompiledState
                  -> Register a
                  -> (Arc -> Register a -> r)
                  -> r
replaceOrRegister us dot k = Hunt.FST.Register.replaceOrRegister us (dotBase dot) $
                             \arc base' -> k arc (dot {
                                                       dotBase     = base'
                                                     , dotRendered = HashSet.union (dotRendered dot) (rendered (arcTarget arc) (ucArcs us))
                                                     , dotOutput   = dotOutput dot `mappend` arcsOut (arcTarget arc) (ucArcs us)
                                                     })

output :: Register a -> ByteString
output dot =
  toLazyByteString (dotInit `mappend` dotOutput dot `mappend` dotEnd)

arcsOut :: StateRef -> Arcs -> Builder
arcsOut st = mconcat . fmap (dotArcFromTo st) . Arcs.arcs

rendered :: StateRef -> Arcs -> HashSet (StateRef, Arc)
rendered st = HashSet.fromList . fmap ((,) st) . Arcs.arcs

dotInit :: Builder
dotInit = mconcat [
      "digraph finite_state_machine {\n"
    , "rankdir=LR\n"
    , "forcelabels=true\n"
    , "node [shape = doublecircle]; 0;\n"
    , "node [shape = circle];\n"
    ]

dotEnd :: Builder
dotEnd = mconcat [
    "}"
  ]

dotArcFromTo :: StateRef -> Arc -> Builder
dotArcFromTo from (Arc label numWords to) =
 mconcat [
   word64Dec to
 , " [xlabel=<<font color=\"red\">"
 , word32Dec numWords
 , "</font>>];\n"
 , word64Dec from
 , " -> "
 , word64Dec to
 , " [ label=\""
 , correctedLabel
 , "\" ];\n"
 ]
 where
   correctedLabel = if (to == 0) || (label == 0)
                    then "\\$"
                    else char8 (chr (fromIntegral label))
