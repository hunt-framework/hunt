module Main where

import           Data.Monoid
import           Test.Framework
import           Test.Framework.Providers.HUnit
--import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
--import           Test.QuickCheck
import qualified Data.Map                            as M
import           Holumbus.Common
import           Holumbus.Interpreter.Interpreter

import           Holumbus.Utility                    (isRight)

-- ----------------------------------------------------------------------------

main :: IO ()
main = defaultMainWithOpts
       [ testCase "Interpreter:            insert" insertTestEmpty
       ] mempty


-- | check DmPrefixTree
insertTestEmpty :: Assertion
insertTestEmpty = do
  (res, _env) <- testRunCmd batchCmd
  True @?= isRight res

testRunCmd :: Command -> IO (Either CmdError CmdRes, Env)
testRunCmd cmd = do
  env <- initEnv emptyIndexer emptyOptions
  res <- runCmd env cmd
  return (res, env)

wordList :: WordList
wordList = M.fromList $ [("hallo", [1,5,10])]

wrds :: Words
wrds = M.fromList $ [("default", wordList)]

doc :: Document
doc = Document "id::1" (M.fromList [("name", "Chris"), ("alter", "30")])

insertCmd, searchCmd, batchCmd :: Command
insertCmd = Insert doc wrds
searchCmd = Search "d"
batchCmd  = Sequence [insertCmd, searchCmd]
