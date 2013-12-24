module Holumbus.Index.Schema.Normalize.Position 
where

import           Data.Maybe          (fromMaybe)
import qualified Data.Text           as T
import           Data.Text           (Text)
import           Text.Read
import qualified Data.List           as L

-- | Check if value is a valid position
-- a valid position has a format like "double double"
-- a space is seperating the long/lat values
isPosition :: Text -> Bool
isPosition p 
  = case split of
      [Just _, Just _] -> True
      _                -> False       
  where
  split = map (\v -> readMaybe (T.unpack v) :: Maybe Double) $ T.splitOn "-" p


-- | Normalizer with "-" as seperator
--   used space here before, but thats a problem within queries
-- Format "double double"
-- XXX normalize length -> we need int normalization first
normalize :: Text -> Text
normalize p 
  = case split of
     [loB, laB] -> T.pack . concat $ zipWith (\lo la -> [lo,la]) loB laB
     _          -> error "failure: invalid format: check that before!"
  where
  split = map format $ T.splitOn "-" p
  format = dec2bin . toInt . toDouble 
  -- XXX use int normalizer here later
  toInt      :: Double -> Integer
  toInt    v = floor (v * 10000000)
   
  toDouble   :: Text -> Double
  toDouble v = fromMaybe 0.0 $ readMaybe (T.unpack v)


denormalize :: Text -> Text
denormalize pos 
  = T.concat [ T.pack . show $ d1, "-", T.pack . show $ d2 ]
  where
  (d1,d2) = ( fromIntegral i1 / 10000000
            , fromIntegral i2 / 10000000
            ) :: (Double,Double)
  (i1,i2) = ( bin2dec . odds $ sPos
            , bin2dec . evens $ sPos
            )
  odds (x:xs) = x : evens xs
  odds _     = []

  evens xs = odds (drop 1 xs)
  sPos = T.unpack pos



bin2dec :: String -> Integer
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where 
    c2i c = if c == '0' then 0 else 1

dec2bin :: Integer -> String
dec2bin = map i2c . reverse . L.unfoldr decomp
    where 
    decomp n = if n == 0 then Nothing else Just(n `mod` 2, n `div` 2)
    i2c i = if i == 0 then '0' else '1' 

