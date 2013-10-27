import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck2
import Test.HUnit
--import Test.QuickCheck
--import Holumbus.Index.Index
--import Holumbus.Index.Proxy.ContextIndex
--import Holumbus.Index.InvertedIndex (TestIndex)
import Holumbus.Index.Test

main :: IO ()
main = defaultMainWithOpts
       [ testCase "prefixtree => index.insert" testInsert1
--       , testCase "invIndex => index.insert" testInsert2
       ] mempty


testInsert1 :: Assertion
testInsert1 = True @?= insertTestPIx

--testInsert2 :: Assertion
--testInsert2 = True @?= insertTestInvIx

--testInsert :: Assertion
--testInsert = reverse [1, 2, 3] @?= [3, 2, 1]
--
