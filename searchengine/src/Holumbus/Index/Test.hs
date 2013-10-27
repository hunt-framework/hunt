{-# LANGUAGE DataKinds #-}
-- i have to put the test cases here, since the test-module
-- won't compile because of this ghc bug we are dealing with
module Holumbus.Index.Test 
( insertTestPIx
--, insertTestCPIx
, insertTestInvIx)
where

import qualified Holumbus.Index.Index                as Ix
import qualified Holumbus.Index.PrefixTreeIndex      as PIx
import qualified Holumbus.Index.ComprPrefixTreeIndex as CPIx
import qualified Holumbus.Index.InvertedIndex        as InvIx
import qualified Holumbus.Index.Proxy.ContextIndex   as ConIx
import           Holumbus.Common.BasicTypes
import           Holumbus.Common.Occurrences         (Occurrences, singleton, Positions, singletonPos)
import           Holumbus.Common.Compression
import           Holumbus.Common.Document            (Document(..))
import qualified Data.Map                            as M


insertTestPIx :: Bool
insertTestPIx = insertTest 
                (Ix.empty::(PIx.DmPrefixTree Positions)) 
                "test" 
                (singleton 1 1)

--insertTestCPIx :: Bool
--insertTestCPIx = insertTest (Ix.empty::(CPIx.ComprOccPrefixTree CompressedPositions)) "test" (deflateOcc $ singleton 1 1)

insertTestInvIx :: Bool
insertTestInvIx = insertTest (Ix.empty::(InvIx.InvertedIndex Occurrences)) "test" (singleton 1 1)

-- | Check if insert works
insertTest emptyIndex k v = v == nv
  where
  [(nk,nv)] = (Ix.search PrefixNoCase k $ Ix.insert k v emptyIndex)

--------------------------------------------------------------------------------------
-- helper
mkWordList :: WordList
mkWordList = M.fromList $ [("hallo", [1,5,10])]

mkWords :: Words
mkWords = M.fromList $ [("default", mkWordList)]

mkDoc :: Document
mkDoc = Document "id::1" (M.fromList [("name", "Chris"), ("alter", "30")])


