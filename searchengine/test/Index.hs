import Data.Monoid
import Test.Framework
import Test.Framework.Providers.HUnit
--import Test.Framework.Providers.QuickCheck2
import Test.HUnit
--import Test.QuickCheck
import Holumbus.Index.Index
import Holumbus.Index.Proxy.ContextIndex


main :: IO ()
main = defaultMainWithOpts
       [ testCase "index.insert" testInsert
       ] mempty

testInsert :: Assertion
testInsert = reverse [1, 2, 3] @?= [3, 2, 1]
--
