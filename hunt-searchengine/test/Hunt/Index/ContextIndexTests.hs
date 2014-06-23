module Hunt.Index.ContextIndexTests
(contextIndexTests)
where

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.Providers.HUnit
import           Test.HUnit                    hiding (Test)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Hunt.Common.BasicTypes
import           Hunt.Common.Occurrences

import qualified Hunt.ContextIndex              as ConIx

import           Hunt.TestHelper
import           Hunt.Index.TestHelper


contextIndexTests :: [Test]
contextIndexTests
      -- XXX todo: test schema insertion on contextinsert. (ignored right now)
    = [ testCase     "ContextIndex:     insert context"    test_insert_cx
      , testCase     "ContextIndex:     delete context"    test_delete_cx
      , testCase     "ContextIndex:     insertList"        test_insertlist
      ]

-- ----------------------------------------------------------------------------
-- helpers
-- ----------------------------------------------------------------------------

-- | check insert context on ContextIndex
test_insert_cx :: Assertion
test_insert_cx
    = True @?= length after == 1 && head after == cxName
    where
     cxName = "context"
     (ConIx.ContextIndex m _ _) = insertCx cxName
     after = ConIx.contexts m

test_delete_cx :: Assertion
test_delete_cx
    = True @?= length before == 1 && length after == 0
    where
      context = "context"
      cix@(ConIx.ContextIndex m _ _) = insertCx context
      before  = ConIx.contexts m
      (ConIx.ContextIndex m' _ _ ) = ConIx.deleteContext context cix
      after   = ConIx.contexts m'

-- | check insert on ContextIndex
test_insertlist :: Assertion
test_insertlist
  = do
    let (ConIx.ContextIndex cix _ _) = insertCx "context"
    cix2 <- ConIx.insertWithCx merge "context" "word" newElem cix
    [(_, insertedElem)] <- ConIx.searchWithCx PrefixNoCase "context" "word" cix2
    True @?= newElem == insertedElem
  where
  newElem = occOne



