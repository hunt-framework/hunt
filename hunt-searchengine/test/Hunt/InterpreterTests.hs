module Hunt.InterpreterTests where

import           System.Directory
import           System.IO

import           Control.Applicative
import           Control.Exception
import           Control.Monad.Error
import           Data.Fixed                           (div', mod')
import           Data.Text                            (Text, pack)

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Text.Printf                          (printf)

import           Hunt.ClientInterface
import           Hunt.Common
import           Hunt.Common.Document
import           Hunt.DocTable.HashedDocTable         (Documents)
import           Hunt.Interpreter
import           Hunt.Query.Ranking
import           Hunt.Utility
import           TestHelper
-- ----------------------------------------------------------------------------

interpreterTests :: [Test]
interpreterTests = 
  -- general test cases
  [ testCase "Interpreter: insert"                     test_insert
  , testCase "Interpreter: search case-insensitive"    test_search_nocase
  , testCase "Interpreter: search case-insensitive"    test_search_nocase2
  , testCase "Interpreter: search case-sensitive"      test_search_case
  , testCase "Interpreter: search case-sensitive"      test_search_case2
  , testCase "Interpreter: phrase case-insensitive"    test_phrase_nocase
  , testCase "Interpreter: phrase case-insensitive"    test_phrase_nocase2
  , testCase "Interpreter: phrase case-sensitive"      test_phrase_case
  , testCase "Interpreter: phrase case-sensitive"      test_phrase_case2
  , testCase "Interpreter: a little bit of everything" test_everything
  -- XXX: still a lot of cases uncovered!

  -- test normalization
  , testCase "Interpreter: norma case-insensitive"     test_norm_search_nocase
  , testCase "Interpreter: norma case-insensitive"     test_norm_search_nocase2
  , testCase "Interpreter: norma case-sensitive"       test_norm_search_case
  , testCase "Interpreter: norma case-sensitive"       test_norm_search_case2
  , testCase "Interpreter: n.phrase case-insensitive"  test_norm_phrase_nocase
  , testCase "Interpreter: n.phrase case-insensitive"  test_norm_phrase_nocase2
  , testCase "Interpreter: n.phrase case-sensitive"    test_norm_phrase_case
  , testCase "Interpreter: n.phrase case-sensitive"    test_norm_phrase_case2
  -- date search specific tests
  , testCase "Interpreter: date context"               test_dates

  -- position search specific tests
  , testCase "Interpreter: geo context"                test_geo
  , testCase "Interpreter: geo context range"          test_geo2
  , testCase "Interpreter: geo context range_a"        test_geo2a
  , testCase "Interpreter: geo context range2"         test_geo3

  -- test binary serialization
  , testCase "Interpreter: store/load index"           test_binary
  , testCase "Interpreter: store/load schema"          test_binary2
  , testProperty "Interpreter: position range query"   prop_position_range
  ]

-- -----------------------------------------------------------
-- Helper (for this test suite)

type TestEnv  = HuntEnv (Documents Document)
type TestCM a = Hunt    (Documents Document) a

rankConfig :: DocumentWrapper e => RankConfig e
rankConfig = defaultRankConfig

testCmd :: Command -> IO (Either CmdError CmdResult)
testCmd cmd = fst <$> testRunCmd cmd

testRunCmd :: Command -> IO (Either CmdError CmdResult, TestEnv)
testRunCmd cmd = do
  env <- initHunt :: IO DefHuntEnv
  res <- runCmd env cmd
  return (res, env)

-- evaluate CM and check the result
testCM' :: Bool -> TestCM () -> Assertion
testCM' b int = do
  env <- initHunt :: IO DefHuntEnv
  res <- runHunt int env
  (if b then isRight else isLeft) res @? "unexpected interpreter result: " ++ show res

-- evaluate CM and check if it yields a result
-- allows for a whole sequence of commands with tests inbetween
-- the interpreter can fail prematurely
testCM :: TestCM () -> Assertion
testCM = testCM' True

-- uris of the search results
searchResultUris :: CmdResult -> [URI]
searchResultUris = map uri . lrResult . crRes

search :: Query -> Int -> Int -> Command
search q o m = setResultOffset o . setMaxResults m . cmdSearch $ q

-- Do something with a temporary file and delete it afterwards
withTmpFile :: (FilePath -> IO a) -> IO a
withTmpFile io = do
  tmpDir <- getTemporaryDirectory
  -- XXX: file exists afterwards!
  --      hacky, but I don't want to deal with generating names etc.
  (file, h) <- openTempFile tmpDir "huntix"
  hClose h -- we just want the filename
  io file `finally` whenM (doesFileExist file) (removeFile file)

-- | default test setup used in most tests
defaultTestSetup :: [Command]
defaultTestSetup
    = [ insertDefaultContext
      , cmdInsertDoc brainDoc
      ]

defaultTestSetup' :: [Command] -> [Command]
defaultTestSetup' cmds = defaultTestSetup ++ cmds

defaultTestSetup'' :: Command -> [Command]
defaultTestSetup'' cmd = defaultTestSetup ++ [cmd]

-- fancy functions
-- characters were chosen without any reason
(@@@) :: Command -> (CmdResult -> IO b) -> TestCM b
a @@@ f = execCmd a >>= liftIO . f

(@@=) :: Command -> CmdResult -> TestCM ()
a @@= b = a @@@ (@?=b)


-- -----------------------------------------------------------
-- General Interpreter API tests

-- just checks the general workflow cx->doc->search
test_insert :: Assertion
test_insert = do
  (res, _env) <- testRunCmd . cmdSequence
                 $ defaultTestSetup
  True @=? isRight res

--
-- Word Search
--

-- insert document and search for it: case insensitive
test_search_nocase :: Assertion
test_search_nocase = do
  res <- testCmd . cmdSequence
         $ defaultTestSetup''
         $ search (setNoCaseSearch $ qWord "Bra") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res

test_search_nocase2 :: Assertion
test_search_nocase2 = do
  res <- testCmd . cmdSequence
         $ defaultTestSetup''
         $ search (setNoCaseSearch $ qWord "bra") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res

-- insert document and search for it: case sensitive
test_search_case :: Assertion
test_search_case = do
  res <- testCmd . cmdSequence
         $ defaultTestSetup''
         $ search (qWord "Bra") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res

test_search_case2 :: Assertion
test_search_case2 = do
  res <- testCmd . cmdSequence
         $ defaultTestSetup''
         $ search (qWord "bra") 0 1000
  [] @=? (searchResultUris . fromRight) res


--
-- Phrase Search
--

-- insert document and search for it: case insensitive
test_phrase_nocase :: Assertion
test_phrase_nocase = do
  res <- testCmd . cmdSequence
         $ defaultTestSetup''
         $ search (setNoCaseSearch $ qPhrase "Brain") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res

test_phrase_nocase2 :: Assertion
test_phrase_nocase2 = do
  res <- testCmd . cmdSequence
         $ defaultTestSetup''
         $ search (setNoCaseSearch $ qPhrase "brain") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res

-- insert document and search for it: case sensitive
test_phrase_case :: Assertion
test_phrase_case = do
  res <- testCmd . cmdSequence
         $ defaultTestSetup''
         $ search (qPhrase "Brain") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res

test_phrase_case2 :: Assertion
test_phrase_case2 = do
  res <- testCmd . cmdSequence
         $ defaultTestSetup''
         $ search (qPhrase "brain") 0 1000
  [] @=? (searchResultUris . fromRight) res

-- -----------------------------------------------------------
-- test application of normalization


-- | test setup used in nomralizer tests
normalizerTestSetup :: [Command]
normalizerTestSetup
    = [ cmdInsertContext "default" (ContextSchema Nothing [cnUpperCase] 1 True ctText)
      , cmdInsertDoc brainDoc
      ]

normalizerTestSetup' :: [Command] -> [Command]
normalizerTestSetup' cmds = defaultTestSetup ++ cmds

normalizerTestSetup'' :: Command -> [Command]
normalizerTestSetup'' cmd = defaultTestSetup ++ [cmd]

--
-- Word search
--

-- insert document and search for it: case insensitive
test_norm_search_nocase :: Assertion
test_norm_search_nocase = do
  res <- testCmd . cmdSequence
         $ normalizerTestSetup''
         $ search (setNoCaseSearch $ qWord "Bra") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res

test_norm_search_nocase2 :: Assertion
test_norm_search_nocase2 = do
  res <- testCmd . cmdSequence
         $ normalizerTestSetup''
         $ search (setNoCaseSearch $ qWord "bra") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res

-- insert document and search for it: case sensitive
test_norm_search_case :: Assertion
test_norm_search_case = do
  res <- testCmd . cmdSequence
         $ normalizerTestSetup''
         $ search (qWord "Bra") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res

-- NOTE: uppercase normalizer makes Case/NoCase irrelevant -> its the same
test_norm_search_case2 :: Assertion
test_norm_search_case2 = do
  res <- testCmd . cmdSequence
         $ normalizerTestSetup''
         $ search (qWord "bra") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res

--
-- Phrase Search
--

-- insert document and search for it: case insensitive
test_norm_phrase_nocase :: Assertion
test_norm_phrase_nocase = do
  res <- testCmd . cmdSequence
         $ normalizerTestSetup''
         $ search (setNoCaseSearch $ qPhrase "Brain") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res

test_norm_phrase_nocase2 :: Assertion
test_norm_phrase_nocase2 = do
  res <- testCmd . cmdSequence
         $ normalizerTestSetup''
         $ search (setNoCaseSearch $ qPhrase "brain") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res

-- insert document and search for it: case sensitive
test_norm_phrase_case :: Assertion
test_norm_phrase_case = do
  res <- testCmd . cmdSequence
         $ normalizerTestSetup''
         $ search (qPhrase "Brain") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res

test_norm_phrase_case2 :: Assertion
test_norm_phrase_case2 = do
  res <- testCmd . cmdSequence
         $ normalizerTestSetup''
         $ search (qPhrase "brain") 0 1000
  ["test://0"] @=? (searchResultUris . fromRight) res


-- -----------------------------------------------------------
-- test binary serialization

test_binary :: Assertion
test_binary = withTmpFile $ \tmpfile -> testCM $ do
  -- create contexts
  insertDateContext       @@= ResOK
  insertDefaultContext    @@= ResOK
  insertGeoContext        @@= ResOK
  -- insert two docuemnts
  cmdInsertDoc dateDoc          @@= ResOK
  cmdInsertDoc geoDoc           @@= ResOK
  -- searching for documents - expecting to find them
  search (setContexts ["datecontext"] (setNoCaseSearch $ qWord "2013-01-01")) 0 10
    @@@ ((@?= ["test://1"]) . searchResultUris)
  search (setContexts ["geocontext"] (setNoCaseSearch $ qWord "53.60000-10.00000")) 0 10
    @@@ ((@?= ["test://2"]) . searchResultUris)
  -- store index
  cmdStoreIndex tmpfile            @@= ResOK
  -- reset index
  cmdDeleteDoc "test://1"       @@= ResOK
  cmdDeleteDoc "test://2"       @@= ResOK
  -- searching for documents - expecting to find none
  search (setContexts ["datecontext"] (setNoCaseSearch $ qWord "2013-01-01")) 0 10
    @@@ ((@?= []) . searchResultUris)
  search (setContexts ["geocontext"] (setNoCaseSearch $ qWord "53.60000-10.00000")) 0 10
    @@@ ((@?= []) . searchResultUris)
  -- loading previously stored index
  cmdLoadIndex tmpfile          @@= ResOK
  -- searching for documents - expecting to find them,
  -- since we found them before we stored the index
  search (setContexts ["datecontext"] (setNoCaseSearch $ qWord "2013-01-01")) 0 10
    @@@ ((@?= ["test://1"]) . searchResultUris)
  search (setContexts ["geocontext"] (setNoCaseSearch $ qWord "53.60000-10.00000")) 0 10
    @@@ ((@?= ["test://2"]) . searchResultUris)

test_binary2 :: Assertion
test_binary2 = withTmpFile $ \tmpfile -> testCM $ do
  -- create contexts
  insertDateContext       @@= ResOK
  insertDefaultContext    @@= ResOK
  insertGeoContext        @@= ResOK
  -- insert two docuemnts
  cmdInsertDoc dateDoc          @@= ResOK
  -- searching for documents - first should be valid second should be invalid
  search (setContexts ["datecontext"] (setNoCaseSearch $ qWord "2013-01-01")) 0 10
    @@@ ((@?= ["test://1"]) . searchResultUris)
  (search (setContexts ["datecontext"] (setNoCaseSearch $ qWord "invalid")) 0 10
    @@@ const (assertFailure "date validation failed"))
        `catchError` const (return ())

  -- store index
  cmdStoreIndex tmpfile         @@= ResOK
  cmdLoadIndex  tmpfile         @@= ResOK
  -- searching for documents - first should be valid second should be invalid
  search (setContext "datecontext" (setNoCaseSearch $ qWord "2013-01-01")) 0 10
    @@@ ((@?= ["test://1"]) . searchResultUris)
  (search (setContext "datecontext" (setNoCaseSearch $ qWord "invalid")) 0 10
    @@@ const (assertFailure "date validation failed after store/load index"))
        `catchError` const (return ())


-- -----------------------------------------------------------
-- index specific tests

test_dates :: Assertion
test_dates = testCM $ do
  -- create contexts
  insertDateContext       @@= ResOK
  insertDefaultContext    @@= ResOK
  -- insert date containing document
  cmdInsertDoc dateDoc          @@= ResOK
  -- searching for date
  search (setContext "datecontext" (setNoCaseSearch $ qWord "2013-01-01")) 0 10
    @@@ ((@?= ["test://1"]) . searchResultUris)


test_geo :: Assertion
test_geo = testCM $ do
  -- create contexts
  insertGeoContext       @@= ResOK
  insertDefaultContext   @@= ResOK
  -- insert date containing document
  cmdInsertDoc geoDoc          @@= ResOK
  -- searching for date
  search (setContext "geocontext" (setNoCaseSearch $ qWord "53.60000-10.00000")) 0 10
    @@@ ((@?= ["test://2"]) . searchResultUris)

test_geo2 :: Assertion
test_geo2 = testCM $ do
  -- create contexts
  insertGeoContext       @@= ResOK
  insertDefaultContext   @@= ResOK
  -- insert date containing document
  cmdInsertDoc geoDoc          @@= ResOK
  -- searching for date
  search (setContext "geocontext" (qRange "1-1" "80-80")) 0 10
    @@@ ((@?= ["test://2"]) . searchResultUris)

test_geo2a :: Assertion
test_geo2a = testCM $ do
  -- create contexts
  insertGeoContext       @@= ResOK
  insertDefaultContext   @@= ResOK

  cmdInsertDoc (geoDoc' "89.63-2.75") @@= ResOK

  search (setContext "geocontext" (qRange "9.40-2.25" "89.25-87.88")) 0 10
    @@@ ((@?= []) . searchResultUris)

test_geo3 :: Assertion
test_geo3 = testCM $ do
  -- create contexts
  insertGeoContext       @@= ResOK
  insertDefaultContext   @@= ResOK
  -- insert date containing document
  cmdInsertDoc geoDoc          @@= ResOK
  -- searching for date
  search (setContext "geocontext" (qRange "-80--80" "1-1")) 0 10
    @@@ ((@?= []) . searchResultUris)

  search (setContext "geocontext" (qRange "60--80" "70--80")) 0 10
    @@@ ((@?= []) . searchResultUris)

-- fancy - equivalent to 'test_alot' plus additional tests
test_everything :: Assertion
test_everything = testCM $ do
  -- insert into non-existent context results in an error
  (cmdInsertDoc brainDoc
    @@@ const (assertFailure "insert into non-existent context succeeded"))
        `catchError` const (return ())
  -- insert context succeeds
  insertDefaultContext
    @@= ResOK

  -- inserting the same context again fails
  (insertDefaultContext
    @@@ const (assertFailure "inserting a context twice succeeded"))
        `catchError` const (return ())

  -- insert yields the correct result value
  cmdInsertDoc brainDoc
    @@= ResOK

  -- searching "Brain" leads to the doc
  search (setNoCaseSearch $ qWord "Brain") os pp
    @@@ ((@?= ["test://0"]) . searchResultUris)
  -- case-sensitive search too
  search (qWord "Brain") os pp
    @@@ ((@?= ["test://0"]) . searchResultUris)
  -- case-sensitive search yields no result
  search (qWord "brain") os pp
    @@@ ((@?= []) . searchResultUris)

  -- insert with default does not update the description
  (cmdInsertDoc brainDocUpdate
    @@@ const (assertFailure "inserting twice succeeded"))
        `catchError` const (return ())
  -- search yields the old description
  search (qWord "Brain") os pp
    @@@ ((@?= adDescr brainDoc) . desc . head . lrResult . crRes)

  -- update the description
  cmdUpdateDoc brainDocUpdate
    @@= ResOK
  -- search yields >merged< description
  search (qWord "Brain") os pp
    @@@ ((@?= adDescr brainDocMerged) . desc . head . lrResult . crRes)

  -- delete return the correct result value
  cmdDeleteDoc ("test://0")
    @@= ResOK
  -- the doc is gone
  search (setNoCaseSearch $ qWord "Brain") os pp
    @@@ ((@?= []) . searchResultUris)
  where
  os = 0
  pp = 1000

getFraction :: Double -> Double
getFraction x = (signum x) * (x - (Prelude.fromInteger $  x `div'` 1))

isInRect :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
--isInRect ne sw p = (unzip ^>> Control.Monad.join (***) (\x -> x == sort x) >>> uncurry (&&)) [ne, p, sw]
isInRect (x1,y1) (x3,y3) (x2,y2) = x1 <= x2 && x2 <= x3 && y1 <= y2 && y2 <= y3

toText :: (Double, Double) -> Text
toText (lat, lon) = (pack $ printf "%f" lat) <> "-" <> (pack $ printf "%f" lon)

prop_position_range :: Double -> Double -> Double -> Double -> (Double, Double) -> Property
prop_position_range x1' x2' x3' x4' (lon', lat') = monadicIO $ do
  res <- run $ do
    env <- initHunt :: IO DefHuntEnv
    res' <- flip runHunt env $ do
      _ <- execCmd insertDefaultContext
      _ <- execCmd insertGeoContext
      _ <- execCmd $ cmdInsertDoc $ geoDoc' $ toText p
      execCmd $ search (setContext "geocontext" (qRange (toText nw) (toText se))) 0 10
    -- print $ (show [nw, se, p]) ++ (show $ searchResultUris $ fromRight res') ++ show isIn
    return res'
  Test.QuickCheck.Monadic.assert $ isIn == (not $ null $ searchResultUris $ fromRight res)
  where
  [x1, x2, x3, x4, lon, lat] = map (abs . (`mod'` 90)) [x1', x2', x3', x4', lon', lat']
  nw = (min x1 x3, min x2 x4)
  se = (max x1 x3, max x2 x4)
  p = (x1 + getFraction lon, x2 + getFraction lat)
  isIn = isInRect nw se p
