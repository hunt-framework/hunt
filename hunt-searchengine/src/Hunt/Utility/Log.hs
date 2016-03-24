{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}


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

  default logShow :: (Show e) => e -> String
  logShow = show

-- ----------------------------------------------------------------------------
