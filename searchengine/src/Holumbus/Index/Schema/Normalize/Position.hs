module Holumbus.Index.Schema.Normalize.Position
( isPosition
, normalize
, denormalize
)
where

import qualified Data.Text           as T
import           Data.Text           (Text)
import qualified Data.List           as L
import           Data.Either         (lefts)

import           Text.Parsec
import           Control.Applicative hiding ((<|>))
import           Text.ParserCombinators.Parsec

-- ----------------------------------------------------------------------------
-- validator

-- | Checks if value is a valid position
-- a valid position has a format like "double-double" aka. "latitude-longitude"
isPosition :: Text -> Bool
isPosition pos = isRight . parse position "" $ T.unpack pos
  where
  isRight = null . lefts . return

-- | Parse a coordinate
position :: Parser (Double, Double)
position = do 
  lat  <- latitude
  _ <- char '-'
  long <- longitude
  return (lat,long)

-- | Parse a latitude value
latitude :: Parser Double
latitude = do 
  pos <- double
  if pos > -90 && pos < 90
    then return pos
    else fail "latitude out of bounds"

-- | Parse a longitude value
longitude :: Parser Double
longitude = do 
  pos <- double
  if pos > -180 && pos < 180
    then return pos
    else fail "longitude out of bounds"

-- ----------------------------------------------------------------------------
-- normalizer

-- | Normalizes valid position
-- a valid position has a format like "double-double" aka. "latitude-longitude"
normalize :: Text -> Text
normalize pos
  = case parse position "" $ T.unpack pos of
     (Right (la,lo)) -> T.pack $ intersectPos (format la) (format lo)
     (Left _)        -> error "normalize positon: invalid position"
  where
  format   :: Double -> String
  format v = dec2bin $ floor (v * 10000000)


-- | Denormalizes internal position into valid position
-- a valid position has a format like "double-double" aka. "latitude-longitude"
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

-- ----------------------------------------------------------------------------
-- normalizer helper

bin2dec :: String -> Integer
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where
    c2i c = if c == '0' then 0 else 1

dec2bin :: Integer -> String
dec2bin = map i2c . reverse . L.unfoldr decomp
    where
    decomp n = if n == 0 then Nothing else Just(n `mod` 2, n `div` 2)
    i2c i = if i == 0 then '0' else '1'

intersectPos :: String -> String -> String
intersectPos la lo = foldPos' la lo ""
    where
    foldPos' (x:xs) (y:ys) a = foldPos' xs ys (a ++ [x] ++ [y])  
    foldPos' (x:xs) []     a = foldPos' xs "" (a ++ [x] ++ "0") 
    foldPos' []     (y:ys) a = foldPos' "" ys (a ++ "0" ++ [y]) 
    foldPos' []    []      a = a
  
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


