import           Data.Monoid
import           Test.Framework
import           Test.Framework.Providers.HUnit
--import           Test.Framework.Providers.QuickCheck2
import           Test.HUnit
--import           Test.QuickCheck
import qualified Data.Map                            as M
import           Holumbus.Common
import           Holumbus.Interpreter.Interpreter

main :: IO ()
main = defaultMainWithOpts
       [ -- testCase "Interpreter:            insert" insertTest
       ] mempty

{--
 - check interpreter DSL
 -} 
runCmd_ :: Command -> IO ()
runCmd_ c
    = do env0 <- initEnv emptyIndexer emptyOptions
         let eval = runCmd env0
         return ()

-- | check DmPrefixTree
--insertTestEmpty :: Assertion
--insertTestEmpty = [] @?= res
  

mkWordList :: WordList
mkWordList = M.fromList $ [("hallo", [1,5,10])]

mkWords :: Words
mkWords = M.fromList $ [("default", mkWordList)]

mkDoc :: Document
mkDoc = Document "id::1" (M.fromList [("name", "Chris"), ("alter", "30")])

insertCmd = Insert mkDoc mkWords
searchCmd = Search "d"
batchCmd = Sequence [insertCmd,searchCmd] 
