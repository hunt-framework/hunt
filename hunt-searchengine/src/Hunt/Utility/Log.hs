{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Hunt.Utility.Log where

class LogShow e where
  logShow :: e -> String

instance Show e => LogShow e where
  logShow = show
