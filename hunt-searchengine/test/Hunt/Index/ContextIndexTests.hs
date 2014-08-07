module Hunt.Index.ContextIndexTests
(contextIndexTests)
where

import           Data.Maybe
import           Control.Monad

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.Providers.HUnit
import           Test.HUnit                           hiding (Test)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           Hunt.Common.Document

import qualified Hunt.ContextIndex                    as ConIx
import qualified Hunt.DocTable                        as Dt
import           Hunt.TestHelper

contextIndexTests :: [Test]
contextIndexTests
      -- XXX todo: test schema insertion on contextinsert. (ignored right now)
    = [ testCase     "ContextIndex:            insert context"    test_insert_cx
      , testCase     "ContextIndex:            delete context"    test_delete_cx
      , testProperty "ContextIndex:            insertList"        test_insertlist
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
test_insertlist :: Property
test_insertlist
  = monadicIO $ do
    -- insert random documents and docIds
    documents <- pick mkDocuments
    let cxIx = insertCx "context"
    insertData <- pick $ mkInsertList $ documents
    (ConIx.ContextIndex _ dt _) <- ConIx.insertList insertData cxIx
    -- check if docuemnts are in document table
    docsTrue <- foldM (\b doc -> Dt.lookupByURI (uri doc) dt >>= \mid -> return (b && isJust mid)) True documents
    -- check if documents are in index
    -- XXX TODO
    return docsTrue
