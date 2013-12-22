module Main where

import           Control.Applicative
import           Control.Monad.Error
--import           Control.Monad.Trans                  (liftIO)

import qualified Data.Map                             as M
--import           Data.Monoid
import qualified Data.Set                             as S

import           Test.Framework
import           Test.Framework.Providers.HUnit
--import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
--import           Test.QuickCheck

import           Holumbus.Common
import           Holumbus.Common.ApiDocument          as ApiDoc
--import           Holumbus.Common.BasicTypes
import           Holumbus.Interpreter.Command
import           Holumbus.Interpreter.Interpreter
import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Ranking
import           Holumbus.Utility
import           Holumbus.Index.InvertedIndex         (InvertedIndex)
import           Holumbus.DocTable.HashedDocTable     (Documents)
import           Holumbus.Index.Schema.Normalize.Date (rexDates)

-- ----------------------------------------------------------------------------

type TestEnv = Env InvertedIndex (Documents Document)
type TestCM a = CM InvertedIndex (Documents Document) a

rankConfig :: RankConfig e
rankConfig = defaultRankConfig

main :: IO ()
main = defaultMain
       [ testCase "Interpreter: insert"
          test_insertEmpty
       , testCase "Interpreter: insertAndSearch"
          test_insertAndSearch
       , testCase "Interpreter: alot"
          test_alot
       , testCase "Interpreter: fancy"
          test_fancy
       , testCase "Interpreter: date context"
          test_dates
       , testCase "Interpreter: geo context"
          test_geo
       ]

-- | check DmPrefixTree
test_insertEmpty :: Assertion
test_insertEmpty = do
  (res, _env) <- testRunCmd batchCmd
  True @=? isRight res

testRunCmd :: Command -> IO (Either CmdError CmdResult, TestEnv)
testRunCmd cmd = do
  env <- initEnv emptyIndexer rankConfig emptyOptions
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
  { apiDocUri      = uri
  , apiDocIndexMap = M.insert "datecontext" "2013-01-01" ix
  , apiDocDescrMap = dt
  }
  where
  ApiDocument uri ix dt = brainDoc

geoDoc :: ApiDocument
geoDoc = emptyApiDoc
  { apiDocUri      = uri
  , apiDocIndexMap = M.insert "geocontext" "53.60000 10.00000" ix
  , apiDocDescrMap = dt
  }
  where
  ApiDocument uri ix dt = brainDoc



-- example apidoc
brainDocUpdate :: ApiDocument
brainDocUpdate = brainDoc { apiDocDescrMap = descr }
  where
  descr = M.fromList [("name", "Pinky"), ("mission", "ask stupid questions")]

brainDocMerged :: ApiDocument
brainDocMerged = brainDocUpdate { apiDocDescrMap = (apiDocDescrMap brainDocUpdate) `M.union` (apiDocDescrMap brainDoc) }

defaultContextInfo :: (Context, ContextSchema)
defaultContextInfo = ("default", ContextSchema CText "[^ \t\n\r]*" [] 1 True)

insertDefaultContext :: Command
insertDefaultContext = uncurry InsertContext defaultContextInfo

dateContextInfo :: (Context, ContextSchema)
dateContextInfo = ("datecontext", ContextSchema CDate "[0-9]{4}-((0[1-9])|(1[0-2]))-((0[1-9])|([12][0-9])|(3[01]))"   [] 1 True)

insertDateContext :: Command
insertDateContext = uncurry InsertContext dateContextInfo

geoContextInfo :: (Context, ContextSchema)
geoContextInfo = ("datecontext", ContextSchema CPosition "" [] 1 True)

insertGeoContext :: Command
insertGeoContext = uncurry InsertContext geoContextInfo




-- evaluate CM and check the result
testCM' :: Bool -> TestCM () -> Assertion
testCM' b int = do
  env <- initEnv emptyIndexer rankConfig emptyOptions
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


test_dates :: Assertion
test_dates = testCM $ do
  -- create contexts
  insertDateContext       @@= ResOK
  insertDefaultContext    @@= ResOK
  -- insert date containing document
  Insert dateDoc          @@= ResOK
  -- searching for date
  Search (QContext ["datecontext"] (QWord QNoCase "2013-01-01")) 0 10
    @@@ ((@?= ["test://0"]) . searchResultUris)


test_geo :: Assertion
test_geo = testCM $ do
  -- create contexts
  insertGeoContext       @@= ResOK
  insertDefaultContext   @@= ResOK
  -- insert date containing document
  Insert geoDoc          @@= ResOK
  -- searching for date
  Search (QContext ["geocontext"] (QWord QNoCase "53.60000 10.00000")) 0 10
    @@@ ((@?= ["test://0"]) . searchResultUris)



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
    @@@ ((@?= (apiDocDescrMap brainDoc)) . desc . head . lrResult . crRes)

  -- update the description
  Update brainDocUpdate
    @@= ResOK
  -- search yields >merged< description
  Search (QWord QCase "Brain") os pp
    @@@ ((@?= (apiDocDescrMap brainDocMerged)) . desc . head . lrResult . crRes)

  -- delete return the correct result value
  BatchDelete (S.singleton "test://0")
    @@= ResOK
  -- the doc is gone
  Search (QWord QNoCase "Brain") os pp
    @@@ ((@?= []) . searchResultUris)
  where
  os = 0
  pp = 1000
