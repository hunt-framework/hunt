{-# LANGUAGE OverloadedStrings #-}

module Main where
{-- Tests for Normalizers Analyzers Formatters #-}

import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Data.Time
--import           Data.Time.Format
import           System.Locale

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
import           Test.QuickCheck
--import qualified Test.QuickCheck.Monadic                as QM

import qualified Holumbus.Index.Schema                as S
import qualified Holumbus.Index.Schema.Analyze        as A
import qualified Holumbus.Index.Schema.Normalize      as N
import qualified Holumbus.Index.Schema.Normalize.Date as ND

-- ----------------------------------------------------------------------------

main :: IO ()
main = defaultMain
       [
       -- Analyzer tests
         testCase "scanTextRE: text1 "             test_scan_text1
       , testCase "scanTextRE: date inv"           test_scan_date1
       , testCase "scanTextRE: date val"           test_scan_date2
       , testCase "scanTextRE: date val multiple"  test_scan_date3
       , testCase "scanTextRE: date val + inval"   test_scan_date4

       -- Normalizer tests - validation
       , testProperty "typeValidator: text"        prop_validate_text
       , testProperty "typeValidator: int val"     prop_validate_int
       , testProperty "typeValidator: int inv"     prop_validate_int2
       , testProperty "typeValidator: date val"    prop_validate_date
       , testProperty "typeValidator: date inv"    prop_validate_date2

       -- Normalizer data - isAnyDate
       , testProperty "Normalizer:date YYYYMMDD"            prop_isAnyDate
       , testProperty "Normalizer:date 2013-01-01T21:12:12" prop_isAnyDate2
       , testProperty "Normalizer:date 2013"                prop_isAnyDate3
       ]

-- ----------------------------------------------------------------------------
-- normalizer tests -


-- ----------------------------------------------------------------------------
-- normalizer date tests

-- | test with date formatted like "2013-01-01"
-- | XXX everything fails?!?!
prop_isAnyDate :: Gen Bool
prop_isAnyDate = dateYYYYMMDD >>= return . ND.isAnyDate . T.unpack

prop_isAnyDate2 :: Gen Bool
prop_isAnyDate2 = return . ND.isAnyDate $ "2013-01-01T21:12:12"

prop_isAnyDate3 :: Gen Bool
prop_isAnyDate3 = return . ND.isAnyDate $ "2013"

-- | test date normalization
-- XXX
prop_norm_date :: Gen Bool
prop_norm_date = undefined

-- ----------------------------------------------------------------------------
-- normalizer tests - validation

-- | every random text should be a valid text
prop_validate_text :: Gen Bool
prop_validate_text = niceText1 >>= return . (N.typeValidator S.CText)

-- | every integer numbers should be valid numbers
prop_validate_int :: Gen Bool
prop_validate_int = do
  int <- arbitrary :: Gen Integer
  return $ N.typeValidator S.CInt (T.pack . show $ int)

-- | random text should not be considered a valid number
prop_validate_int2 :: Gen Bool
prop_validate_int2 = niceText1 >>= \t -> return $ False == N.typeValidator S.CInt ("a" `T.append` t)

-- | date formated "yyyy-mm-dd" should be valid
prop_validate_date :: Gen Bool
prop_validate_date = dateYYYYMMDD >>= return . (N.typeValidator S.CDate)

-- | random text should not be considered a valid date
prop_validate_date2 :: Gen Bool
prop_validate_date2 = niceText1 >>= \d -> return $ False == N.typeValidator S.CDate d

-- ----------------------------------------------------------------------------
-- scan tests

-- | test general text regex
test_scan_text1 :: Assertion
test_scan_text1 = assert $ length scan == 3
  where
  scan = A.scanTextRE "[^ \t\n\r]*" "w1 w2 w3"

-- | test date regex with invalid date given
test_scan_date1 :: Assertion
test_scan_date1 = assert $ length scan == 0
  where
  scan = A.scanTextRE "[0-9]{4}-((0[1-9])|(1[0-2]))-((0[1-9])|([12][0-9])|(3[01]))" "w1 w2 w3"

-- | test date regex with valid date given
test_scan_date2 :: Assertion
test_scan_date2 = assert $ length scan == 1
  where
  scan = A.scanTextRE "[0-9]{4}-((0[1-9])|(1[0-2]))-((0[1-9])|([12][0-9])|(3[01]))" "2013-01-01"

-- | test date regex with multiple dates given
test_scan_date3 :: Assertion
test_scan_date3 = assert $ length scan == 2
  where
  scan = A.scanTextRE "[0-9]{4}-((0[1-9])|(1[0-2]))-((0[1-9])|([12][0-9])|(3[01]))" "2013-01-01 2012-12-31"

-- | test date regex with date containing string
test_scan_date4 :: Assertion
test_scan_date4 = assert $ (length scan == 2) && (scan !! 1 == "2013-01-01")
  where
  scan = A.scanTextRE "[0-9]{4}-((0[1-9])|(1[0-2]))-((0[1-9])|([12][0-9])|(3[01]))" "2013-01-01 asd 2013-01-01"

-- ----------------------------------------------------------------------------
-- helper

niceText1 :: Gen Text
niceText1 = fmap T.pack . listOf1 . elements $ concat [" ", ['0'..'9'], ['A'..'Z'], ['a'..'z']]

dateYYYYMMDD :: Gen Text
dateYYYYMMDD = arbitrary >>= \x -> return . T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" (newDate x)
  where
  newDate x = addDays (-x) (fromGregorian 2013 12 31)


