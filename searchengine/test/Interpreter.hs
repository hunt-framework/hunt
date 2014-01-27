module Main where

import           Control.Applicative
import           Control.Monad.Error
--import           Control.Monad.Trans                         (liftIO)

import qualified Data.Map                                    as M
--import           Data.Monoid
import qualified Data.Set                                    as S
import           Data.Text                                   (Text)

import           Test.Framework
import           Test.Framework.Providers.HUnit
--import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
--import           Test.QuickCheck

import           Holumbus.Common
import           Holumbus.Common.Document
import           Holumbus.Common.Document.Compression.BZip
import           Holumbus.Common.ApiDocument                 as ApiDoc
--import           Holumbus.Common.BasicTypes
import           Holumbus.Interpreter.Command
import           Holumbus.Interpreter.Interpreter
import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Ranking
import           Holumbus.Utility
--import           Holumbus.Index.InvertedIndex                (InvertedIndex)
import           Holumbus.DocTable.HashedDocTable            (Documents)
--import           Holumbus.Index.Schema.Normalize.Date        (rexDates)

-- ----------------------------------------------------------------------------

type TestEnv = Env (Documents CompressedDoc)
type TestCM a = CM (Documents CompressedDoc) a

rankConfig :: DocumentWrapper e => RankConfig e
rankConfig = defaultRankConfig

main :: IO ()
main = defaultMain
  -- general test cases
  [ testCase "Interpreter: insert"                     test_insertEmpty
  , testCase "Interpreter: insertAndSearch"            test_insertAndSearch
  , testCase "Interpreter: alot"                       test_alot
  , testCase "Interpreter: fancy"                      test_fancy

  -- date search specific tests
  , testCase "Interpreter: date context"               test_dates

  -- postion search specifix tests
  , testCase "Interpreter: geo context"                test_geo
  , testCase "Interpreter: geo context range"          test_geo2
  , testCase "Interpreter: geo context range2"         test_geo3

  -- test binary serialization
  , testCase "Interpreter: store/load index"           test_binary
  , testCase "Interpreter: store/load schema"          test_binary2
  ]

-- | check DmPrefixTree
test_insertEmpty :: Assertion
test_insertEmpty = do
  (res, _env) <- testRunCmd batchCmd
  True @=? isRight res

testRunCmd :: Command -> IO (Either CmdError CmdResult, TestEnv)
testRunCmd cmd = do
  env <- initEnv emptyIndexer rankConfig contextTypes
  res <- runCmd env cmd
  return (res, env)


insertCmd, searchCmd, batchCmd :: Command
insertCmd = Insert brainDoc
searchCmd = Search (QWord QNoCase "d") 1 100
batchCmd  = Sequence [insertDefaultContext, insertCmd, searchCmd]

-- ----------------------------------------------------------------------------

testCmd :: Command -> IO (Either CmdError CmdResult)
testCmd cmd = fst <$> testRunCmd cmd

-- uris of the search results
searchResultUris :: CmdResult -> [URI]
searchResultUris = map uri . lrResult . crRes

-- example apidoc
brainDoc :: ApiDocument
brainDoc = emptyApiDoc
  { apiDocUri      = "test://0"
  , apiDocIndexMap = M.fromList [("default", td)]
  , apiDocDescrMap = descr
  }
  where
  td = "Brain"
  descr = M.fromList [("name", "Brain"), ("mission", "take over the world"), ("legs", "4")]

dateDoc :: ApiDocument
dateDoc = emptyApiDoc
  { apiDocUri      = "test://1"
  , apiDocIndexMap = M.insert "datecontext" "2013-01-01" ix
  , apiDocDescrMap = dt
  }
  where
  ApiDocument _ ix dt = brainDoc

geoDoc :: ApiDocument
geoDoc = emptyApiDoc
  { apiDocUri      = "test://2"
  , apiDocIndexMap = M.insert "geocontext" "53.60000-10.00000" ix
  , apiDocDescrMap = dt
  }
  where
  ApiDocument _ ix dt = brainDoc



-- example apidoc
brainDocUpdate :: ApiDocument
brainDocUpdate = brainDoc { apiDocDescrMap = descr }
  where
  descr = M.fromList [("name", "Pinky"), ("mission", "ask stupid questions")]

brainDocMerged :: ApiDocument
brainDocMerged = brainDocUpdate { apiDocDescrMap = (apiDocDescrMap brainDocUpdate) `M.union` (apiDocDescrMap brainDoc) }

defaultContextInfo :: (Context, ContextSchema)
defaultContextInfo = ("default", ContextSchema Nothing [] 1 True ctText)

insertDefaultContext :: Command
insertDefaultContext = uncurry InsertContext defaultContextInfo

dateContextInfo :: (Context, ContextSchema)
dateContextInfo = ("datecontext", ContextSchema Nothing [] 1 True ctDate)

insertDateContext :: Command
insertDateContext = uncurry InsertContext dateContextInfo

-- XXX: regex ok? examples:
--      correct: 90-180, -90--180, 0.-0., 0.0-0.0, 0.00-0.00
--      wrong:   91-181, -91--181, 01-01
--using context type default now here
--geoRex :: Text
--geoRex = "-?(90(\\.0*)?|[1-7]?[0-9](\\.[0-9]*)?)--?((180(\\.0*)?)|(1[0-7][0-9])|([1-9]?[0-9]))(\\.[0-9]*)?"

geoContextInfo :: (Context, ContextSchema)
geoContextInfo = ("geocontext", ContextSchema Nothing [] 1 True ctPosition)

insertGeoContext :: Command
insertGeoContext = uncurry InsertContext geoContextInfo


-- evaluate CM and check the result
testCM' :: Bool -> TestCM () -> Assertion
testCM' b int = do
  env <- initEnv emptyIndexer rankConfig contextTypes
  res <- runCM int env
  (if b then isRight else isLeft) res @? "unexpected interpreter result: " ++ show res


-- evaluate CM and check if it yields a result
-- allows for a whole sequence of commands with tests inbetween
-- the interpreter can fail prematurely
testCM :: TestCM () -> Assertion
testCM = testCM' True


-- search for inserted doc
-- sequence of commands using the execSeq
test_insertAndSearch :: Assertion
test_insertAndSearch = do
  res <- testCmd . Sequence $
      [ insertDefaultContext
      , Insert brainDoc
      , Search (QWord QNoCase "Brain") 0 1000]
  ["test://0"] @=? (searchResultUris . fromRight) res


-- test a whole sequence with tests inbetween
-- the interpreter can fail prematurely
test_alot :: Assertion
test_alot = testCM $ do
  --throwNYI "user error"
  insCR <- execCmd insertDefaultContext
  liftIO $ ResOK @=? insCR
  insR <- execCmd $ Insert brainDoc
  liftIO $ ResOK @=? insR
  seaR <- execCmd $ Search (QWord QNoCase "Brain") os pp
  liftIO $ ["test://0"] @=? searchResultUris seaR
  seaR2 <- execCmd $ Search (QWord QCase "brain") os pp
  liftIO $ [] @=? searchResultUris seaR2
  where
  os = 0
  pp = 1000


-- fancy functions
-- characters were chosen without any reason
(@@@) :: Command -> (CmdResult -> IO b) -> TestCM b
a @@@ f = execCmd a >>= liftIO . f

(@@=) :: Command -> CmdResult -> TestCM ()
a @@= b = a @@@ (@?=b)

test_binary :: Assertion
test_binary = testCM $ do
  -- create contexts
  insertDateContext       @@= ResOK
  insertDefaultContext    @@= ResOK
  insertGeoContext        @@= ResOK
  -- insert two docuemnts
  Insert dateDoc          @@= ResOK
  Insert geoDoc           @@= ResOK
  -- searching for documents - expecting to find them
  Search (QContext ["datecontext"] (QWord QNoCase "2013-01-01")) 0 10
    @@@ ((@?= ["test://1"]) . searchResultUris)
  Search (QContext ["geocontext"] (QWord QNoCase "53.60000-10.00000")) 0 10
    @@@ ((@?= ["test://2"]) . searchResultUris)
  -- store index
  StoreIx "/tmp/ix"       @@= ResOK
  -- reset index
  Delete "test://1"       @@= ResOK
  Delete "test://2"       @@= ResOK
  -- searching for documents - expecting to find none
  Search (QContext ["datecontext"] (QWord QNoCase "2013-01-01")) 0 10
    @@@ ((@?= []) . searchResultUris)
  Search (QContext ["geocontext"] (QWord QNoCase "53.60000-10.00000")) 0 10
    @@@ ((@?= []) . searchResultUris)
  -- loading previously stored index
  LoadIx "/tmp/ix"        @@= ResOK
  -- searching for documents - expecting to find them,
  -- since we found them before we stored the index
  Search (QContext ["datecontext"] (QWord QNoCase "2013-01-01")) 0 10
    @@@ ((@?= ["test://1"]) . searchResultUris)
  Search (QContext ["geocontext"] (QWord QNoCase "53.60000-10.00000")) 0 10
    @@@ ((@?= ["test://2"]) . searchResultUris)

test_binary2 :: Assertion
test_binary2 = testCM $ do
  -- create contexts
  insertDateContext       @@= ResOK
  insertDefaultContext    @@= ResOK
  insertGeoContext        @@= ResOK
  -- insert two docuemnts
  Insert dateDoc          @@= ResOK
  -- searching for documents - first should be valid second should be invalid
  Search (QContext ["datecontext"] (QWord QNoCase "2013-01-01")) 0 10
    @@@ ((@?= ["test://1"]) . searchResultUris)
  (Search (QContext ["datecontext"] (QWord QNoCase "invalid")) 0 10
    @@@ const (assertFailure "date validation failed"))
        `catchError` const (return ())

  -- store index
  StoreIx "/tmp/ix"       @@= ResOK
  LoadIx "/tmp/ix"        @@= ResOK

  -- searching for documents - first should be valid second should be invalid
  Search (QContext ["datecontext"] (QWord QNoCase "2013-01-01")) 0 10
    @@@ ((@?= ["test://1"]) . searchResultUris)
  (Search (QContext ["datecontext"] (QWord QNoCase "invalid")) 0 10
    @@@ const (assertFailure "date validation failed after store/load index"))
        `catchError` const (return ())

test_dates :: Assertion
test_dates = testCM $ do
  -- create contexts
  insertDateContext       @@= ResOK
  insertDefaultContext    @@= ResOK
  -- insert date containing document
  Insert dateDoc          @@= ResOK
  -- searching for date
  Search (QContext ["datecontext"] (QWord QNoCase "2013-01-01")) 0 10
    @@@ ((@?= ["test://1"]) . searchResultUris)


test_geo :: Assertion
test_geo = testCM $ do
  -- create contexts
  insertGeoContext       @@= ResOK
  insertDefaultContext   @@= ResOK
  -- insert date containing document
  Insert geoDoc          @@= ResOK
  -- searching for date
  Search (QContext ["geocontext"] (QWord QNoCase "53.60000-10.00000")) 0 10
    @@@ ((@?= ["test://2"]) . searchResultUris)

test_geo2 :: Assertion
test_geo2 = testCM $ do
  -- create contexts
  insertGeoContext       @@= ResOK
  insertDefaultContext   @@= ResOK
  -- insert date containing document
  Insert geoDoc          @@= ResOK
  -- searching for date
  Search (QContext ["geocontext"] (QRange "1-1" "80-80")) 0 10
    @@@ ((@?= ["test://2"]) . searchResultUris)


test_geo3 :: Assertion
test_geo3 = testCM $ do
  -- create contexts
  insertGeoContext       @@= ResOK
  insertDefaultContext   @@= ResOK
  -- insert date containing document
  Insert geoDoc          @@= ResOK
  -- searching for date
  Search (QContext ["geocontext"] (QRange "-80--80" "1-1")) 0 10
    @@@ ((@?= []) . searchResultUris)

  Search (QContext ["geocontext"] (QRange "60--80" "70--80")) 0 10
    @@@ ((@?= []) . searchResultUris)

-- fancy - equivalent to 'test_alot' plus additional tests
test_fancy :: Assertion
test_fancy = testCM $ do
  -- insert into non-existent context results in an error
  (Insert brainDoc
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
  Insert brainDoc
    @@= ResOK

  -- searching "Brain" leads to the doc
  Search (QWord QNoCase "Brain") os pp
    @@@ ((@?= ["test://0"]) . searchResultUris)
  -- case-sensitive search too
  Search (QWord QCase "Brain") os pp
    @@@ ((@?= ["test://0"]) . searchResultUris)
  -- case-sensitive search yields no result
  Search (QWord QCase "brain") os pp
    @@@ ((@?= []) . searchResultUris)

  -- insert with default does not update the description
  (Insert brainDocUpdate
    @@@ const (assertFailure "inserting twice succeeded"))
        `catchError` const (return ())
  -- search yields the old description
  Search (QWord QCase "Brain") os pp
    @@@ ((@?= apiDocDescrMap brainDoc) . desc . head . lrResult . crRes)

  -- update the description
  Update brainDocUpdate
    @@= ResOK
  -- search yields >merged< description
  Search (QWord QCase "Brain") os pp
    @@@ ((@?= apiDocDescrMap brainDocMerged) . desc . head . lrResult . crRes)

  -- delete return the correct result value
  BatchDelete (S.singleton "test://0")
    @@= ResOK
  -- the doc is gone
  Search (QWord QNoCase "Brain") os pp
    @@@ ((@?= []) . searchResultUris)
  where
  os = 0
  pp = 1000
