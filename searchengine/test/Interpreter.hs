module Main where

--import           Data.Monoid
import           Test.Framework
import           Test.Framework.Providers.HUnit
--import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
--import           Test.QuickCheck
import qualified Data.Map                         as M
import           Holumbus.Common
import           Holumbus.Interpreter.Interpreter

import           Holumbus.Utility

import           Holumbus.Common.ApiDocument      as ApiDoc
import           Holumbus.Query.Language.Grammar
import           Control.Monad.Trans              (liftIO)

-- ----------------------------------------------------------------------------

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

testRunCmd :: Command -> IO (Either CmdError CmdResult, Env)
testRunCmd cmd = do
  env <- initEnv emptyIndexer emptyOptions
  res <- runCmd env cmd
  return (res, env)


insertCmd, searchCmd, batchCmd :: Command
insertCmd = Insert brainDoc New
searchCmd = Search (Word "d")
batchCmd  = Sequence [insertCmd, searchCmd]

-- ----------------------------------------------------------------------------

testCmd :: Command -> IO (Either CmdError CmdResult)
testCmd cmd = do
  env <- initEnv emptyIndexer emptyOptions
  runCmd env cmd

-- uris of the search results
searchResultUris :: CmdResult -> [URI]
searchResultUris = map uri . crRes

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
testCM' :: Bool -> CM () -> Assertion
testCM' b int = do
  env <- initEnv emptyIndexer emptyOptions
  res <- runCM int env
  (if b then isRight else isLeft) res @? "unexpected interpreter result: " ++ show res


-- evaluate CM and check if it yields a result
-- allows for a whole sequence of commands with tests inbetween
-- the interpreter can fail prematurely
testCM :: CM () -> Assertion
testCM = testCM' True


-- search for inserted doc
-- sequence of commands using the execSeq
test_insertAndSearch :: Assertion
test_insertAndSearch = do
  res <- testCmd . Sequence $
      [ Insert brainDoc New
      , Search (Word "Brain")]
  ["test://0"] @=? (searchResultUris . fromRight) res


-- test a whole sequence with tests inbetween
-- the interpreter can fail prematurely
test_alot :: Assertion
test_alot = testCM $ do
  --throwNYI "user error"
  insR <- execCmd $ Insert brainDoc New
  liftIO $ ResOK @=? insR
  seaR <- execCmd $ Search (Word "Brain")
  liftIO $ ["test://0"] @=? searchResultUris seaR
  seaR2 <- execCmd $ Search (CaseWord "brain")
  liftIO $ [] @=? searchResultUris seaR2


-- fancy functions
-- characters were chosen without any reason
(@@@) :: Command -> (CmdResult -> IO b) -> CM b
a @@@ f = execCmd a >>= liftIO . f

(@@=) :: Command -> CmdResult -> CM ()
a @@= b = a @@@ (@?=b)


-- fancy - equivalent to 'test_alot' plus additional tests
test_fancy :: Assertion
test_fancy = testCM $ do
  -- insert yields the correct result value
  Insert brainDoc New
    @@= ResOK
  -- searching "brain" leads to the doc
  Search (Word "Brain")
    @@@ ((@?= ["test://0"]) . searchResultUris)
  -- case-sensitive search too
  Search (CaseWord "Brain")
    @@@ ((@?= ["test://0"]) . searchResultUris)
  -- case-sensitive search yields no result
  Search (CaseWord "brain")
    @@@ ((@?= []) . searchResultUris)
  -- delete return the correct result value
  Delete "test://0"
    @@= ResOK
  -- the doc is gone
  Search (Word "Brain")
    @@@ ((@?= []) . searchResultUris)
