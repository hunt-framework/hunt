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

import qualified Hunt.Index                     as Ix
import qualified Hunt.ContextIndex              as ConIx
import qualified Hunt.Index.InvertedIndex       as InvIx
import           Hunt.Index.IndexImpl

import           Hunt.TestHelper                ()
import           Hunt.Index.TestHelper


contextIndexTests :: [Test]
contextIndexTests
    = [ testCase     "ContextIndex:     insert context"    test_insert_cx
      , testCase     "ContextIndex:     delete context"    test_delete_cx
      , testCase     "ContextIndex:     insertList"        test_insertlist
      ]

-- | check insert context on ContextIndex
test_insert_cx :: Assertion
test_insert_cx = "test" @?= insertedContext
  where
  [insertedContext] = ConIx.contexts ix
  ix :: ConIx.ContextMap Occurrences
  ix = ConIx.insertContext "test" (mkIndex (Ix.empty :: InvIx.InvertedIndex Occurrences)) ConIx.empty

test_delete_cx :: Assertion
test_delete_cx = undefined

-- | check insert on ContextIndex
test_insertlist :: Assertion
test_insertlist
  = do
    let cix = ConIx.insertContext "context" (mkIndex impl) emptyIndex
    cix2 <- ConIx.insertWithCx merge "context" "word" newElem cix
    [(_, insertedElem)] <- ConIx.searchWithCx PrefixNoCase "context" "word" cix2
    True @?= newElem == insertedElem
  where
  newElem = occOne
  emptyIndex :: ConIx.ContextMap Occurrences
  emptyIndex = ConIx.empty
  impl       :: InvIx.InvertedIndex Occurrences
  impl       = Ix.empty




