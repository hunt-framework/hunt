{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Holumbus.Utility.Log where

class LogShow e where
  logShow :: e -> String

instance Show e => LogShow e where
  logShow = show
