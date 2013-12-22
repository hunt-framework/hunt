module Holumbus.Index.Schema.Normalize.Position 
where

import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           Data.Text           (Text)
import           Text.Read
import qualified Data.List           as L

import           Holumbus.Common.BasicTypes

-- | regex not used here atm.
--   accepted format: double|double
isPosition :: Text -> Bool
isPosition p 
  = if length split < 2 
    then False
    else case (parsedLong, parsedLat) of
      (Just _, Just _) -> True
      _                -> False       
  where
  split = T.splitOn "|" p
  -- XXX refactor to map or something
  long       = split !! 0
  lat        = split !! 1
  parsedLong = readMaybe (T.unpack long) :: Maybe Double
  parsedLat  = readMaybe (T.unpack lat)  :: Maybe Double

-- | covertts double|double to int that contains both positions
normalizePosition :: Word -> Word
normalizePosition p 
  = if length split < 2 
    then error "failure: list should always be splitable!"
    else res 
  where
  split = T.splitOn "|" p
  -- XXX refactor to map or something
  long       = split !! 0
  lat        = split !! 1
  pLong      = fromMaybe 0.0 $ readMaybe (T.unpack long) :: Double
  pLat       = fromMaybe 0.0 $ readMaybe (T.unpack lat)  :: Double
  (loI, laI) = (floor (pLong * 10000000), floor (pLat * 10000000)) :: (Integer,Integer)
  (loB, laB) = (dec2bin loI, dec2bin laI)
  res        = T.pack $ concat $ zipWith (\lo la -> [lo,la]) loB laB


bin2dec :: String -> Integer
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where 
    c2i c = if c == '0' then 0 else 1

dec2bin :: Integer -> String
dec2bin = map i2c . reverse . L.unfoldr decomp
    where 
    decomp n = if n == 0 then Nothing else Just(n `mod` 2, n `div` 2)
    i2c i = if i == 0 then '0' else '1' 

