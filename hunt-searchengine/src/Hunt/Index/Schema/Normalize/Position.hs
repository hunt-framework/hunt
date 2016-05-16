-- ----------------------------------------------------------------------------
{- |
  Normalization and validation for geographic positions.
-}
-- ----------------------------------------------------------------------------
{-# LANGUAGE BangPatterns #-}
module Hunt.Index.Schema.Normalize.Position
       ( normalize
       , denormalize
       , isPosition
       , parsePosition
       , position
       )
where

import           Data.Attoparsec.Text
import           Data.Bits
import           Data.Int
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import qualified Data.Text.Internal.Fusion        as Stream
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Builder           as TB
import qualified Data.Text.Lazy.Builder.RealFloat as TB

-- ------------------------------------------------------------
-- validator
-- ------------------------------------------------------------

parsePosition :: Text -> Either String (Double, Double)
parsePosition = parseOnly position

-- | Checks if value is a valid position.
--   A valid position has a format like "double-double"/"latitude-longitude".
isPosition :: Text -> Bool
isPosition pos | Right _ <- parsePosition pos = True
               | otherwise                    = False

-- | Parse a coordinate.
position :: Parser (Double, Double)
position = do
  lat  <- latitude
  _ <- char '-'
  long <- longitude
  return (lat,long)

-- | Parse a latitude value.
latitude :: Parser Double
latitude = do
  pos <- double
  if pos > -90 && pos < 90
    then return pos
    else fail "latitude out of bounds"

-- | Parse a longitude value.
longitude :: Parser Double
longitude = do
  pos <- double
  if pos > -180 && pos < 180
    then return pos
    else fail "longitude out of bounds"

-- ------------------------------------------------------------
-- normalizer
-- ------------------------------------------------------------

-- | Normalizes valid position
--   A valid position has a format like "double-double"/"latitude-longitude".
normalize :: Text -> Text
normalize pos
  | Right (la, lo) <- parsePosition pos =
      intersectPos (format la) (format lo)
  | otherwise = error "normalize position: invalid position"
  where format :: Double -> Int32
        format v
          | v >= 0 = setBit (round (v * 10000000)) 31
          | otherwise = clearBit (round (v * 10000000)) 31

-- | Denormalizes internal position into valid position.
--   A valid position has a format like "double-double"/"latitude-longitude".
denormalize :: Text -> Text
denormalize pos = toText $
  TB.formatRealFloat TB.Generic (Just 7) d1
  `mappend` TB.singleton '-'
  `mappend` TB.formatRealFloat TB.Generic (Just 7) d2
  where
    d1 :: Double
    d1 | testBit i1 31 = fromIntegral (clearBit i1 31) / 10000000
       | otherwise     = - fromIntegral i1 / 10000000

    d2 :: Double
    d2 | testBit i2 31 = fromIntegral (clearBit i2 31) / 10000000
       | otherwise     = - fromIntegral i2 / 10000000

    (i1, i2) = unintersectPos pos

    toText = TL.toStrict . TB.toLazyText


data P = P !Int !Int32 !Int32

-- | Merge two lists starting with an element of the first list, then one
--   of the second list and so on.
--   Works like @concat . zipWith (\a b -> a:b:[])@, but pads with zeros.
intersectPos :: Int32 -> Int32 -> Text
intersectPos x0 y0 = Stream.unstream go
  where go = Stream.Stream step (P 0 x0 y0) 32
          where step (P n x y)
                  | n == 64 = Stream.Done
                  | otherwise =
                    let !a  = if testBit x 31 then '1' else '0'
                        !x' = x `unsafeShiftL` 1
                    in Stream.Yield a (P (n + 1) y x')
{-# INLINE intersectPos #-}

unintersectPos :: Text -> (Int32, Int32)
unintersectPos s0 = (x', y')
  where
    P _ x' y' = T.foldl' f (P 0 0 0) s0

    f (P n x y) c | even n = P (n + 1) (cbit c x) y
                  | otherwise = P (n + 1) x (cbit c y)

    cbit '0' n = clearBit (n `unsafeShiftL` 1) 0
    cbit '1' n = setBit (n `unsafeShiftL` 1) 0
    cbit _ _ = error "unintersectPosPos.cbit"
