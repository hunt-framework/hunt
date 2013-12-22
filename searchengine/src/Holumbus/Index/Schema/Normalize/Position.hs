module Holumbus.Index.Schema.Normalize.Position 
where

import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           Data.Text           (Text)
import           Text.Read
import qualified Data.List           as L

import           Holumbus.Common.BasicTypes

-- | Check if value is a valid position
-- a valid position has a format like "double double"
-- a space is seperating the long/lat values
isPosition :: Text -> Bool
isPosition p 
  = case split of
      [Just _, Just _] -> True
      _                -> False       
  where
  split = map (\v -> readMaybe (T.unpack v) :: Maybe Double) $ T.splitOn " " p


-- | Normalizer with space as seperator
-- Format "double double"
-- XXX normalize length -> we need int normalization first
normalizePosition :: Word -> Word
normalizePosition p 
  = case split of
     [loB, laB] -> T.pack . concat $ zipWith (\lo la -> [lo,la]) loB laB
     _          -> error "failure: invalid format: check that before!"
  where
  split = map format $ T.splitOn " " p
  format = dec2bin . toInt . toDouble 
  -- XXX use int normalizer here later
  toInt      :: Double -> Integer
  toInt    v = floor (v * 10000000)
   
  toDouble   :: Text -> Double
  toDouble v = fromMaybe 0.0 $ readMaybe (T.unpack v)


bin2dec :: String -> Integer
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where 
    c2i c = if c == '0' then 0 else 1

dec2bin :: Integer -> String
dec2bin = map i2c . reverse . L.unfoldr decomp
    where 
    decomp n = if n == 0 then Nothing else Just(n `mod` 2, n `div` 2)
    i2c i = if i == 0 then '0' else '1' 

