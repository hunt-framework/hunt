{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE RankNTypes #-}
module Hunt.FST.Packed where

import           Control.DeepSeq
import           Hunt.FST.Arcs (Arc(..), Arcs)
import           Hunt.FST.Register
import           Hunt.FST.Types
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Word

data Entry = E !StateRef !Weight

type FstSize = Word64

data Register = Register {
    regStates  :: !(HashMap Arcs Entry)
  , regFstSize :: !FstSize
  }

instance NFData Entry where
  rnf (E s w) = s `deepseq` w `deepseq` ()

instance NFData (Register) where
  rnf r = regStates r
          `deepseq` regFstSize r
          `deepseq` ()

empty :: Register
empty = Register {
    regStates  = HashMap.empty
  , regFstSize = 0
  }
{-# INLINE empty #-}

replaceOrRegister :: UncompiledState -> Register -> (Arc -> Register -> a) -> a
replaceOrRegister (UncompiledState label arcs) register k =
  case HashMap.lookup arcs (regStates register) of
    Just (E stateRef w) -> k (Arc label w stateRef) register
    Nothing             -> k (Arc label weight (regFstSize register')) register'
  where
    register' = register {
        regStates  = HashMap.insert arcs (E (regFstSize register + 1) 1) (regStates register)
      , regFstSize = regFstSize register + 1
      }
    weight = 1
{-# INLINE replaceOrRegister #-}
