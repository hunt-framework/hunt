{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Fox.Types.Score where

newtype Score = SC { unScore :: Float }
              deriving (Eq, Ord, Num, Fractional, Show)

noScore :: Score
noScore = SC 0.0

defScore :: Score
defScore = SC 1.0
