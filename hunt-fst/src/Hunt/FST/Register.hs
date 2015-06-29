module Hunt.FST.Register where

import Hunt.FST.Arcs
import Hunt.FST.Types

data UncompiledState a
  = UncompiledState { ucLabel :: !Label
                    , ucArcs  :: !Arcs
                    , ucOut   :: !(Maybe a)
                    } deriving (Eq, Show)
