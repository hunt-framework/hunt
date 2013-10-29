module Main where

import           Data.Monoid
import           Test.Framework
import           Test.Framework.Providers.HUnit
--import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
--import           Test.QuickCheck
import qualified Data.Map                            as M
import qualified Data.Text                           as T
import           Holumbus.Common
import           Holumbus.Interpreter.Interpreter
import           Data.Either                         (rights)

-- ----------------------------------------------------------------------------

main :: IO ()
main = defaultMainWithOpts
       [ testCase "Interpreter:            insert" insertTestEmpty
       ] mempty


-- | check DmPrefixTree
insertTestEmpty :: Assertion
insertTestEmpty = do
  (res, _env) <- testRunCmd batchCmd
  True @?= eitherIsRight res
  --True @?= notEmpty res
  where
  eitherIsRight res = 1 == (length $ rights [res])
  --notEmpty res = 1 == (length . _theDocs . head $ rights [res])

testRunCmd :: Command -> IO (Either CmdError CmdRes, Env)
testRunCmd cmd = do
  env <- initEnv emptyIndexer emptyOptions
  res <- runCmd env cmd
  return (res, env)

mkWordList :: WordList
mkWordList = M.fromList $ [(T.pack "hallo", [1,5,10])]

mkWords :: Words
mkWords = M.fromList $ [(T.pack "default", mkWordList)]

mkDoc :: Document
mkDoc = Document (T.pack "id::1") (M.fromList [(T.pack "name", T.pack "Chris"), (T.pack "alter", T.pack "30")])

insertCmd, searchCmd, batchCmd :: Command
insertCmd = Insert mkDoc mkWords
searchCmd = Search "d"
batchCmd = Sequence [insertCmd,searchCmd]
