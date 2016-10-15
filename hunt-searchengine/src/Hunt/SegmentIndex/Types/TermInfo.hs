module Hunt.SegmentIndex.Types.TermInfo where

import           Control.DeepSeq

data TermInfo =
  TermInfo { tiNumOccs    :: !Int
           , tiOccsOffset :: !Int
           } deriving (Eq, Show)

instance NFData TermInfo where
  rnf ti = ti `seq` ()
