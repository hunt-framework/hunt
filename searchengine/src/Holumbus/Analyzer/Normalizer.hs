module Holumbus.Analyzer.Normalizer
  ( normalizerMapping
  , typeValidator
  )
where

import           Control.Monad

import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Holumbus.Common.BasicTypes
import           Holumbus.Common.Schema

import           Data.Char                   (isDigit)
import           Data.Function               (on)
import           Data.Maybe
import           Data.Ratio                  ((%))
import           Data.Time                   (Day, DiffTime, UTCTime (..),
                                              addUTCTime, fromGregorian)

import           Text.Regex.XMLSchema.String

-- ----------------------------------------------------------------------------

normalizerMapping :: CNormalizer -> Word -> Word
normalizerMapping o = case o of
    NormUpperCase -> T.toUpper
    NormLowerCase -> T.toLower
    -- TODO: date has to be a valid schema date
    --       maybe disable json parsing or use a separate enum
    NormDate      -> normalizeDate

-- ----------------------------------------------------------------------------

-- | Normalize a date representation to store in the index or search for.
normalizeDate :: Text -> Text
normalizeDate = T.pack . normDateRep . showDate . normDate . readAnyDate . T.unpack
  where
  -- TODO: to GMT / eliminate timezone
  normDate :: Date -> Date
  normDate = id
  -- TODO: general normalizer for the date representation
  normDateRep :: String -> String
  normDateRep = filter (`elem` "-T")

-- ----------------------------------------------------------------------------

typeValidator :: CType -> Text -> Bool
typeValidator t = case t of
    CText -> const True
    CInt  -> const True
    -- XXX: maybe use more rigid anyDate'?
    CDate -> isAnyDate . T.unpack

-- ----------------------------------------------------------------------------

-- | Checks if the string is a date representation (syntactically).
isAnyDate :: String -> Bool
isAnyDate s = any ($ s) $ map fst safeDateReaders

-- Same as 'isAnyDate'' but also checks if @(showDate . readAnyDate)@ produces the same result.
isAnyDate' :: String -> Bool
isAnyDate' s = isAnyDate s && ((==s) . showDate . readAnyDate) s

-- | Unsafe 'readAnyDateM'.
readAnyDate :: String -> Date
readAnyDate = fromJust . readAnyDateM

-- | Try to read a date.
readAnyDateM :: String -> Maybe Date
readAnyDateM s = fmap head . mapM readDateM $ safeDateReaders
  where
  readDateM :: (String -> Bool, String -> Date) -> Maybe Date
  readDateM (v, r) = guard (v s) >> return (r s)

-- | Tuples of date validator and reader.
safeDateReaders :: [(String -> Bool, String -> Date)]
safeDateReaders = zip validators readers
  where
  validators :: [String -> Bool]
  validators = [  isDateTime,   isDate,   isGYearMonth,   isGYear,   isGMonthDay,   isGMonth,   isGDay]
  readers    :: [String -> Date]
  readers    = [readDateTime, readDate, readGYearMonth, readGYear, readGMonthDay, readGMonth, readGDay]

-- ----------------------------------------------------------------------------

-- source hxt-xmlschema/src/Text/XML/HXT/XMLSchema/W3CDataTypeCheck.hs

-- ----------------------------------------
--
-- Days are represented here
-- as in ISO 8601:2000 Second Edition:
--    ISO (International Organization for Standardization).
--    Representations of dates and times, second edition, 2000-12-15.
--
-- NOT as in
-- ISO 8601
--    ISO (International Organization for Standardization).
--    Representations of dates and times, 1988-06-15.
--
-- The main difference is dealing with year 0.
-- in the older ISO standard, this is excluded and
-- "-0001" is the representation of year 1 Before Common Era "-1 BCE".
-- In the latter standard "0000" represents "-1 BCE" and "-0001" represents "-2 BCE"

data Date =
    Date { _dUTCTime :: UTCTime
         , _dTZ      :: MaybeTimeZone
         }
    deriving (Show)

type MaybeTimeZone = Maybe Seconds

type Seconds = Int

instance Eq Date where
    (==) = (==) `on` toUTCTime

instance Ord Date where
    compare = compare `on` toUTCTime

mkDateTime :: Day -> DiffTime -> MaybeTimeZone -> Date
mkDateTime d t z
    = Date (UTCTime d t) z

toUTCTime :: Date -> UTCTime
toUTCTime (Date d Nothing) = d
toUTCTime (Date d (Just tz)) = addUTCTime (fromInteger . toInteger $ tz) d

-- ----------------------------------------------------------------------------

isDateTime, isDate, isTime, isGYearMonth, isGYear, isGMonthDay, isGMonth, isGDay :: String -> Bool
[isDateTime, isDate, isTime, isGYearMonth, isGYear, isGMonthDay, isGMonth, isGDay]
    = map matchRE rexDates


rexDates :: [Regex]
rexDates
    = map rex [dateTime, date, time, gYearMonth, gYear, gMonthDay, gMonth, gDay]
    where
      dateTime   = ymd ++ "T" ++ hms ++ tz
      time       =               hms ++ tz
      date       = ymd               ++ tz
      gYearMonth = ym                ++ tz
      gYear      = y                 ++ tz

      gMonthDay  = "--" ++ m2 ++ "-" ++ t2 ++ tz
      gMonth     = "--" ++ m2              ++ tz
      gDay       = "--"       ++ "-" ++ t2 ++ tz

      y     = "-?" ++ y4'
      ym    = y           ++ "-" ++ m2
      ymd   = ym                       ++ "-" ++ t2

      hms   = alt (h2 ++ ":" ++ i2 ++ ":" ++ s2 ++ fr)
                  ("24:00:00" ++ opt ".0+")             -- 24:00 is legal

      tz    = opt (alt tz0 "Z")
      tz0   = (alt "\\-" "\\+") ++ tz1
      tz1   = alt (h13 ++ ":" ++ i2) "14:00:00"

      m2    = alt "0[1-9]" "1[0-2]"                     -- Month
      t2    = alt "0[1-9]" (alt "[12][0-9]" "3[01]")    -- Tag
      h2    = alt "[01][0-9]" "2[0-3]"                  -- Hour
      i2    = "[0-5][0-9]"                              -- mInute
      s2    = i2                                        -- Seconds

{-                                                      -- this conforms to ISO 8601 from 1988
      y1    = "000[1-9]"                                -- "0000" isn't a year, "-0001" represents "-1 BCE"
      y2    = "00[1-9][0-9]"                            -- leading 0-s are only allowd for year < 1000
      y3    = "0[1-9][0-9]{2}"
      y4    = "[1-9][0-9]{3,}"
      y4'   = alt y4 $ alt y3 $ alt y2 y1
-- -}

-- {-                                                   -- this conforms to ISO 8601 Second Edition from 2000
      y4    = "[0-9]{4}"                                -- year "0000" is legal and represents "-1 BCE"
      y4'   = opt "[1-9][0-9]*" ++ y4
-- -}

      fr    = opt ".[0-9]+"

      h13   = alt "0[0-9]" "1[0-3]"

      opt x     = "(" ++ x ++ ")?"
      alt x1 x2 = "((" ++ x1 ++ ")|(" ++ x2 ++ "))"


-- ----------------------------------------------------------------------------

readDate
  , readGYearMonth
  , readGYear
  , readGMonthDay
  , readGMonth
  , readGDay :: String -> Date

readDate       = readDate' readYearMonthDayS
readGYearMonth = readDate' readYearMonthS
readGYear      = readDate' readYearS
readGMonthDay  = readDate' readMonthDayS
readGMonth     = readDate' readMonthS
readGDay       = readDate' readDayS


readTimeZone :: String -> MaybeTimeZone
readTimeZone ""
    = Nothing
readTimeZone "Z"
    = Just 0
readTimeZone (s : xs)
    = Just .
      ( if s == '-' then negate else id ) .
      readZone $ xs
    where
      readZone s'
          = 60 * (60 * read hs + read ms)
          where
            (hs, (_ : ms)) = span (/= ':') s'

readYearMonthDayS :: String -> (Day, String)
readYearMonthDayS s0
    = (fromGregorian (sign $ read year) (read month) (read day), rest)
    where
      (sign,          s ) = if head s0 == '-'
                            then (negate, tail s0)
                            else (id,          s0)
      (year,  (_ : rest1)) = span (/= '-') s
      (month, (_ : rest2)) = span (/= '-') rest1
      (day,        rest  ) = span isDigit rest2

readYearMonthS :: String -> (Day, String)
readYearMonthS s0
    = (fromGregorian (sign $ read year) (read month) 1, rest)
    where
      (sign,          s ) = if head s0 == '-'
                            then (negate, tail s0)
                            else (id,          s0)
      (year,  (_ : rest1)) = span (/= '-') s
      (month,      rest  ) = span isDigit rest1

readYearS :: String -> (Day, String)
readYearS s0
    = (fromGregorian (sign $ read year) 1 1, rest)
    where
      (sign,          s ) = if head s0 == '-'
                            then (negate, tail s0)
                            else (id,          s0)
      (year,       rest  ) = span isDigit s

readMonthDayS :: String -> (Day, String)
readMonthDayS s0
    = (fromGregorian 1 (read month) (read day), rest)
    where
      (month, (_ : rest1)) = span isDigit . drop 2 $ s0
      (day,        rest  ) = span isDigit rest1

readMonthS :: String -> (Day, String)
readMonthS s0
    = (fromGregorian 1 (read month) 1, rest)
    where
      (month,       rest ) = span isDigit . drop 2 $ s0

readDayS :: String -> (Day, String)
readDayS s0
    = (fromGregorian 1 1 (read day), rest)
    where
      (day,         rest ) = span isDigit . drop 3 $ s0

readHourMinSec :: String -> DiffTime
readHourMinSec s
    = fromInteger (60 * (60 * read hours + read minutes))
      +
      fromRational (readDecimal seconds)
    where
      (hours,   (_ :    rest)) = span (/= ':') s
      (minutes, (_ : seconds)) = span (/= ':') rest

readDateTime :: String -> Date
readDateTime s
    = mkDateTime day (readHourMinSec time) (readTimeZone zone)
    where
      (day,  (_ : rest)) = readYearMonthDayS s
      (time,       zone) = span (\ x -> isDigit x || x `elem` ":.") rest


readDate' :: (String -> (Day, String)) -> String -> Date
readDate' read' s
    = mkDateTime day nullTime (readTimeZone zone)
    where
      (day, zone) = read' s

nullTime :: DiffTime
nullTime = fromInteger 0

nullDay :: Day
nullDay = fromGregorian 1 1 1

-- ----------------------------------------

-- | Reads a decimal from a string

readDecimal :: String -> Rational
readDecimal ('+':s) = readDecimal' s
readDecimal ('-':s) = negate $ readDecimal' s
readDecimal      s  = readDecimal' s

-- | Helper function to read a decimal from a string
readDecimal' :: String -> Rational
readDecimal' s
  | f == 0    = (n % 1)
  | otherwise = (n % 1) + (f % (10 ^ (toInteger $ length fs)))
  where
  (ns, fs') = span (/= '.') s
  fs = drop 1 fs'

  f :: Integer
  f | null fs   = 0
    | otherwise = read fs
  n :: Integer
  n | null ns   = 0
    | otherwise = read ns

-- --------------------
-- the show must go on

showDateTime :: Date -> String
showDateTime (Date d tz)
    = ymd ++ "T" ++ hms ++ showTimeZone tz
    where
      (ymd : hms : _) = words . show $ d

showDate' :: (String -> String) -> Date -> String
showDate' fmt (Date d tz)
    = fmt ymd ++ showTimeZone tz
    where
      (ymd : _) = words . show $ d

dropRev :: Int -> String -> String
dropRev i = reverse . drop i . reverse

showDate :: Date -> String
showDate = showDate' $ id

showGYearMonth :: Date -> String
showGYearMonth = showDate' $ dropRev 3

showGYear :: Date -> String
showGYear = showDate' $ dropRev 6

showGMonthDay :: Date -> String
showGMonthDay = showDate' $ ('-' :) . reverse . take 6 . reverse

showGMonth :: Date -> String
showGMonth = showDate' $ ('-' :) . reverse . take 3 . drop 3 . reverse

showGDay :: Date -> String
showGDay = showDate' $ ('-' :) . ('-' :) . reverse . take 3 . reverse

-- it's
showTime :: Date -> String
showTime (Date d tz)
    = hms ++ showTimeZone tz
    where
      (_ymd : hms : _) = words . show $ d

showTimeZone :: MaybeTimeZone -> String
showTimeZone Nothing
    = ""
showTimeZone (Just s)
    | s == 0    = "Z"
    | s >  0    = '+' : showHourMin s
    | otherwise = '-' : showHourMin (negate s)

showHourMin :: Int -> String
showHourMin s0
    = showDec 2 (s `div` 60) ++ ":" ++ showDec 2 (s `mod` 60)
    where
      s = s0 `div` 60

showDec :: Int -> Int -> String
showDec n = reverse . toStr n
    where
      toStr 0 _ = ""
      toStr l i = show (i `mod` 10) ++ toStr (l-1) (i `div` 10)

-- ----------------------------------------------------------------------------

-- | Creates a regex from a string
rex :: String -> Regex
rex regex
  | isZero ex = error $ "syntax error in regexp " ++ show regex ++ "."
  | otherwise = ex
  where
  ex = parseRegex regex

-- ----------------------------------------------------------------------------
