{-# LANGUAGE TypeFamilies #-}
module Hunt.FST.Register where

import Hunt.FST.Arcs
import Hunt.FST.Types

data UncompiledState = UncompiledState {
    ucLabel  :: !Label
  , ucArcs   :: !Arcs
  } deriving (Eq, Show)

type ReplaceOrRegister s r = UncompiledState -> s -> (Arc -> s -> r) -> r

class Register1 a where
  type Output a
  empty  :: a
  output :: a -> Output a
  replaceOrRegister :: ReplaceOrRegister a r
