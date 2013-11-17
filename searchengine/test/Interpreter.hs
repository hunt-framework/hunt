module Main where

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
insertCmd = Insert brainDoc Default
searchCmd = Search (QText NoCase "d") 1 100
batchCmd  = Sequence [insertDefaultContext, insertCmd, searchCmd]

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
  descr = M.fromList [("name", "Brain"), ("mission", "take over the world"), ("legs", "4")]

-- example apidoc
brainDocUpdate :: ApiDocument
brainDocUpdate = brainDoc { apiDocDescrMap = descr }
  where
  descr = M.fromList [("name", "Pinky"), ("mission", "ask stupid questions")]

brainDocMerged :: ApiDocument
brainDocMerged = brainDocUpdate { apiDocDescrMap = (apiDocDescrMap brainDocUpdate) `M.union` (apiDocDescrMap brainDoc) }

defaultContextInfo :: (Context, (CType, CRegex, [CNormalizer], CWeight))
defaultContextInfo = ("default", (CText, "[^ \t\n\r]*", [], 1))

insertDefaultContext :: Command
insertDefaultContext = uncurry InsertContext defaultContextInfo


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
      [ insertDefaultContext
      , Insert brainDoc Default
      , Search (QText NoCase "Brain") 0 1000]
  ["test://0"] @=? (searchResultUris . fromRight) res


-- test a whole sequence with tests inbetween
-- the interpreter can fail prematurely
test_alot :: Assertion
test_alot = testCM $ do
  --throwNYI "user error"
  insCR <- execCmd insertDefaultContext
  liftIO $ ResOK @=? insCR
  insR <- execCmd $ Insert brainDoc Default
  liftIO $ ResOK @=? insR
  seaR <- execCmd $ Search (QText NoCase "Brain") os pp
  liftIO $ ["test://0"] @=? searchResultUris seaR
  seaR2 <- execCmd $ Search (QText Case "brain") os pp
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


-- fancy - equivalent to 'test_alot' plus additional tests
test_fancy :: Assertion
test_fancy = testCM $ do
  -- insert into non-existent context results in an error
  (Insert brainDoc Default
    @@@ const (assertFailure "insert into non-existent context succeeded"))
        `catchError` const (return ())
  -- insert context succeeds
  insertDefaultContext
    @@= ResOK

  -- insert yields the correct result value
  Insert brainDoc Default
    @@= ResOK

  -- searching "Brain" leads to the doc
  Search (QText NoCase "Brain") os pp
    @@@ ((@?= ["test://0"]) . searchResultUris)
  -- case-sensitive search too
  Search (QText Case "Brain") os pp
    @@@ ((@?= ["test://0"]) . searchResultUris)
  -- case-sensitive search yields no result
  Search (QText Case "brain") os pp
    @@@ ((@?= []) . searchResultUris)

  -- insert with default does not update the description
  Insert brainDocUpdate Default
    @@= ResOK
  -- search yields the old description
  Search (QText Case "Brain") os pp
    @@@ ((@?= (apiDocDescrMap brainDoc)) . desc . head . lrResult . crRes)

  -- update the description
  Insert brainDocUpdate Update
    @@= ResOK
  -- search yields >merged< description
  Search (QText Case "Brain") os pp
    @@@ ((@?= (apiDocDescrMap brainDocMerged)) . desc . head . lrResult . crRes)

  -- delete return the correct result value
  BatchDelete (S.singleton "test://0")
    @@= ResOK
  -- the doc is gone
  Search (QText NoCase "Brain") os pp
    @@@ ((@?= []) . searchResultUris)
  where
  os = 0
  pp = 1000
