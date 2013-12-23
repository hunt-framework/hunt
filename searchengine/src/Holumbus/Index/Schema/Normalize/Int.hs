module Holumbus.Index.Schema.Normalize.Int
where

import           Data.Maybe          (isJust, fromMaybe)
import qualified Data.Text           as T
import           Data.Text           (Text)
import           Text.Read

import           Holumbus.Common.BasicTypes

-- | Check if value is a valid position
-- a valid position has a format like "double double"
-- a space is seperating the long/lat values
isInt :: Text -> Bool
isInt v = isJust mv && v == smv 
  where
  -- check for parseable int
  mv  = readMaybe (T.unpack v) :: Maybe Int
  -- check for overflows
  smv = T.pack . show $ fromMaybe 0 mv

normalizeInt :: Word -> Word
normalizeInt i 
  = T.concat [pfx, zeros, nr] 
  where
  (pfx,nr) = if T.take 1 i == "-"  
             then ("0", T.drop 1 i) 
             else ("1", i)
  elems    = 20 - (T.length nr)
  zeros    = T.pack $ foldr (\_ xs -> '0':xs) "" [1..elems]
   

