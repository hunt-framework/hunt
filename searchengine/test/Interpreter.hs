module Main where

import           Control.Monad.Trans                  (liftIO)

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
import           Holumbus.Interpreter.Command
import           Holumbus.Interpreter.Interpreter
import           Holumbus.Query.Language.Grammar
import           Holumbus.Utility
import           Holumbus.Index.InvertedIndex         (InvertedIndex)
import           Holumbus.DocTable.HashedDocuments    (Documents)
-- ----------------------------------------------------------------------------

type TestEnv = Env InvertedIndex (Documents Document)
type TestCM a = CM InvertedIndex (Documents Document) a

main :: IO ()
main = defaultMain
       [ testCase "Interpreter: insert"
          test_insertEmpty
       , testCase "Interpreter: insertAnalyzed"
          test_insertAnalyzed
       , testCase "Interpreter: insertAndSearch"
          test_insertAndSearch
       , testCase "Interpreter: alot"
          test_alot
       , testCase "Interpreter: fancy"
          test_fancy
       ]

-- | check DmPrefixTree
test_insertEmpty :: Assertion
test_insertEmpty = do
  (res, _env) <- testRunCmd batchCmd
  True @=? isRight res

testRunCmd :: Command -> IO (Either CmdError CmdResult, TestEnv)
testRunCmd cmd = do
  env <- initEnv emptyIndexer emptyOptions
  res <- runCmd env cmd
  return (res, env)


insertCmd, searchCmd, batchCmd :: Command
insertCmd = Insert brainDoc New
searchCmd = Search (Word "d") 1 100
batchCmd  = Sequence [insertCmd, searchCmd]

-- ----------------------------------------------------------------------------

testCmd :: Command -> IO (Either CmdError CmdResult)
testCmd cmd = do
  env <- initEnv emptyIndexer emptyOptions
  runCmd env cmd

-- uris of the search results
searchResultUris :: CmdResult -> [URI]
searchResultUris = map uri . lrResult . crRes

-- example apidoc
brainDoc :: ApiDocument
brainDoc = emptyApiDoc
  { apiDocUri      = "test://0"
  , apiDocIndexMap = M.fromList [("default", Right td)]
  , apiDocDescrMap = descr
  }
  where
  td = TextData
    { idContent  = "Brain"
    , idMetadata = md
    }
  md = IndexMetadata
    { imAnalyzer = DefaultAnalyzer
    }
  descr = M.fromList [("name", "Brain"), ("mission", "take over the world")]


-- insert example apidoc
test_insertAnalyzed :: Assertion
test_insertAnalyzed = do
  res <- testCmd $
      Insert brainDoc New
  True @=? isRight res


-- evaluate CM and check the result
testCM' :: Bool -> TestCM () -> Assertion
testCM' b int = do
  env <- initEnv emptyIndexer emptyOptions
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
      [ Insert brainDoc New
      , Search (Word "Brain") 1 1000]
  ["test://0"] @=? (searchResultUris . fromRight) res


-- test a whole sequence with tests inbetween
-- the interpreter can fail prematurely
test_alot :: Assertion
test_alot = testCM $ do
  --throwNYI "user error"
  insR <- execCmd $ Insert brainDoc New
  liftIO $ ResOK @=? insR
  seaR <- execCmd $ Search (Word "Brain") p pp
  liftIO $ ["test://0"] @=? searchResultUris seaR
  seaR2 <- execCmd $ Search (CaseWord "brain") p pp
  liftIO $ [] @=? searchResultUris seaR2
  where
  p = 1
  pp = 1000


-- fancy functions
-- characters were chosen without any reason
(@@@) :: Command -> (CmdResult -> IO b) -> TestCM b
a @@@ f = execCmd a >>= liftIO . f

(@@=) :: Command -> CmdResult -> TestCM ()
a @@= b = a @@@ (@?=b)


-- fancy - equivalent to 'test_alot' plus additional tests
test_fancy :: Assertion
test_fancy = testCM $ do
  -- insert yields the correct result value
  Insert brainDoc New
    @@= ResOK
  -- searching "brain" leads to the doc
  Search (Word "Brain") p pp
    @@@ ((@?= ["test://0"]) . searchResultUris)
  -- case-sensitive search too
  Search (CaseWord "Brain") p pp
    @@@ ((@?= ["test://0"]) . searchResultUris)
  -- case-sensitive search yields no result
  Search (CaseWord "brain") p pp
    @@@ ((@?= []) . searchResultUris)
  -- delete return the correct result value
  BatchDelete (S.singleton "test://0")
    @@= ResOK
  -- the doc is gone
  Search (Word "Brain") p pp
    @@@ ((@?= []) . searchResultUris)
  where
  p = 1
  pp = 1000
