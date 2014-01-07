module Holumbus.Index.Schema.Normalize.Position
{--( isPosition
, normalize
, denormalize
)--}
where

import           Numeric

import           Control.Applicative           hiding ((<|>))

import           Data.Text                     (Text)
import qualified Data.Text                     as T


import           Text.Parsec
import           Text.ParserCombinators.Parsec

import           Holumbus.Utility

-- ----------------------------------------------------------------------------
-- validator

-- | Checks if value is a valid position.
--   A valid position has a format like "double-double"/"latitude-longitude".
isPosition :: Text -> Bool
isPosition pos = isRight . parse position "" $ T.unpack pos

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

-- ----------------------------------------------------------------------------
-- normalizer

-- | Normalizes valid position
--   A valid position has a format like "double-double"/"latitude-longitude".
normalize :: Text -> Text
normalize pos
  = case parse position "" $ T.unpack pos of
     Right (la,lo) -> T.pack $ intersectPos (format la) (format lo)
     Left _        -> error "normalize positon: invalid position"
  where
  format   :: Double -> String
  format v = dec2bin $ floor (v * 10000000)


-- | Denormalizes internal position into valid position.
--   A valid position has a format like "double-double"/"latitude-longitude".
denormalize :: Text -> Text
denormalize pos
  = T.pack . show' d1 . ('-':) . show' d2 $ ""
  where
  (d1,d2) = ( fromIntegral i1 / 10000000
            , fromIntegral i2 / 10000000
            ) :: (Double,Double)
  (i1,i2) = ( bin2dec odds
            , bin2dec evens
            )

  (odds,evens) = oddsAndEvens sPos

  oddsAndEvens o = case o of
    (x1:x2:xs) -> let (y1,y2) = oddsAndEvens xs in (x1:y1,x2:y2)
    (x1:xs)    -> let (y1,y2) = oddsAndEvens xs in (x1:y1,y2)
    []         -> ([],[])

  sPos = T.unpack pos

  show' = showFFloat (Just 7)

-- ----------------------------------------------------------------------------
-- normalizer helper

-- | Decimal representation of a binary encoded string.
--   /NOTE/: The input needs to be valid.
--           @length input >= 2 && all (`elem` "01") input@
bin2dec :: String -> Int
bin2dec (s:i) = sign dec
  where
  dec = fst . head . readInt 2 isbc c2b $ i
  sign = if s == '0' then negate else id

-- | Convert Integer to Binary and normalize result
--   with leading zeros to a length of 32 characters.
--   The first character is the sign: 0 -> negative.
--   /NOTE/: The input needs to be valid.
dec2bin :: Integer -> String
dec2bin i = sign . zeros . bin $ ""
  where
  sign, zeros, bin :: ShowS
  (sign,n) = if i < 0 then (('0':),-i) else (('1':),i)
  bin      = showIntAtBase 2 b2c n
  zeros s  = foldr (\ _ xs -> '0' : xs) s [1..elems]
    where elems = 31 - length s

-- | Merge two lists starting with an element of the first list, then one
--   of the second list and so on.
--   Works like @concat . zipWith (\a b -> a:b:[])@, but pads with zeros.
intersectPos :: String -> String -> String
intersectPos (x:xs) (y:ys) = x   : y   : intersectPos xs ys
intersectPos (x:xs) []     = x   : '0' : intersectPos xs []
intersectPos []     (y:ys) = '0' : y   : intersectPos [] ys
intersectPos _      _      = []

{-
-- ShowS version with accumulator - unnecessary
intersectPos :: String -> String -> ShowS
intersectPos la lo = foldPos' la lo id
    where
    foldPos' (x:xs) (y:ys) a = foldPos' xs ys (\s -> a (x   : y   : s))
    foldPos' (x:xs) []     a = foldPos' xs "" (\s -> a (x   : '0' : s))
    foldPos' []     (y:ys) a = foldPos' "" ys (\s -> a ('0' : y   : s))
    foldPos' []    []      a = a
-}

-- ----------------------------------------------------------------------------
-- Int to binary and vice versa

-- | Binary integer to character.
b2c :: Int -> Char
b2c i = case i of
  0 -> '0'
  1 -> '1'
  _ -> error "b2c i with i `notElem` [0,1]"

-- | Character to binary integer.
c2b :: Char -> Int
c2b o = case o of
  '0' -> 0
  '1' -> 1
  _ -> error "c2b with c `notElem` \"01\""

-- | Is the character a binary number.
isbc :: Char -> Bool
isbc = (`elem` "01")

-- ----------------------------------------------------------------------------
-- parser helper

number :: Parser String
number = many1 digit

plus :: Parser String
plus = char '+' *> number

minus :: Parser String
minus = char '-' <:> number

integer :: Parser String
integer = plus <|> minus <|> number

double :: Parser Double
double = fmap rd $ integer <++> decimal
    where rd      = read :: String -> Double
          decimal = option "" $ char '.' <:> number

(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b
