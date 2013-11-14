module Holumbus.Analyzer.Normalizer
  ( normalizerMapping
  )
where

import qualified Data.Text                   as T

import           Holumbus.Common.BasicTypes
import           Holumbus.Common.Schema

-- ----------------------------------------------------------------------------

normalizerMapping :: CNormalizer -> Word -> Word
normalizerMapping o = case o of
    CUpperCase -> T.toUpper
    CLowerCase -> T.toLower
