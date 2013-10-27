import          Data.Monoid
import          Test.Framework
import          Test.Framework.Providers.HUnit
--import          Test.Framework.Providers.QuickCheck2
import          Test.HUnit
--import          Test.QuickCheck
import qualified Holumbus.Index.Index                as Ix
import qualified Holumbus.Index.PrefixTreeIndex      as PIx
import qualified Holumbus.Index.ComprPrefixTreeIndex as CPIx
import qualified Holumbus.Index.InvertedIndex        as InvIx
--import qualified Holumbus.Index.Proxy.ContextIndex   as ConIx
import           Holumbus.Common.BasicTypes
import           Holumbus.Common.Occurrences         (Occurrences, singleton, Positions)
import           Holumbus.Common.Compression
import           Holumbus.Common.Document            (Document(..))
import qualified Data.Map                            as M


main :: IO ()
main = defaultMainWithOpts
       [ testCase "index.DmPrefixTree:         func: insert" insertTestPIx
       , testCase "index.InvertedIndex:        func: insert" insertTestInvIx
       ] mempty

{--
 - check if element is inserted in insert operation
 -} 

-- | general check function
insertTest emptyIndex k v = v == nv
  where
  [(nk,nv)] = (Ix.search PrefixNoCase k $ Ix.insert k v emptyIndex)

-- | check DmPrefixTree
insertTestPIx :: Assertion
insertTestPIx 
  = True @?= insertTest 
    (Ix.empty::(PIx.DmPrefixTree Positions)) 
    "test" 
    (singleton 1 1)

-- | check ComprOccPrefixTree
{--insertTestCPIx :: Assertion
insertTestCPIx 
  = True @?= insertTest 
    (Ix.empty::(CPIx.ComprOccPrefixTree CompressedPositions)) 
    "test" 
    (deflateOcc $ singleton 1 1)
--}
--
-- | check InvertedIndex
insertTestInvIx :: Assertion
insertTestInvIx 
  = True @?= insertTest 
    (Ix.empty::(InvIx.InvertedIndex Occurrences)) 
    "test" 
    (singleton 1 1)

-- | check ContextIndex
-- TODO




--------------------------------------------------------------------------------------
-- helper
--

mkWordList :: WordList
mkWordList = M.fromList $ [("hallo", [1,5,10])]

mkWords :: Words
mkWords = M.fromList $ [("default", mkWordList)]

mkDoc :: Document
mkDoc = Document "id::1" (M.fromList [("name", "Chris"), ("alter", "30")])


