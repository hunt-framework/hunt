{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

-- ----------------------------------------------------------------------------
{- |
  Logging auxiliary functions.

  The 'LogShow' is an alternative 'Show' instance for a custom representation in logs.
  It uses the 'Show' instance by default.
-}
-- ----------------------------------------------------------------------------

module Hunt.Utility.Log where

-- | Alternative 'Show' instance for a custom representation in logs.
--   It uses the 'Show' instance by default.
class LogShow e where
  logShow :: e -> String

instance Show e => LogShow e where
  logShow = show
