module Main where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Error
import qualified Data.Map                              as M
--import           Data.Default

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit

import           Hunt.Common
import           Hunt.Common.ApiDocument               as ApiDoc
import           Hunt.Common.Document
import           Hunt.Interpreter.Command
import           Hunt.Interpreter
import           Hunt.Query.Language.Grammar
import           Hunt.Query.Ranking
import           Hunt.Utility
import           Hunt.DocTable.HashedDocTable          (Documents)

-- ----------------------------------------------------------------------------

type TestEnv  = HuntEnv (Documents Document)
type TestCM a = Hunt    (Documents Document) a

rankConfig :: DocumentWrapper e => RankConfig e
rankConfig = defaultRankConfig

main :: IO ()
main = defaultMain
  [ testCase "Ranking: Boosting"                    test_ranking
  ]

testRunCmd :: Command -> IO (Either CmdError CmdResult, TestEnv)
testRunCmd cmd = do
  env <- initHunt :: IO DefHuntEnv
  res <- runCmd env cmd
  return (res, env)

-- ----------------------------------------------------------------------------

testCmd :: Command -> IO (Either CmdError CmdResult)
testCmd cmd = fst <$> testRunCmd cmd

-- uris of the search results
searchResultUris :: CmdResult -> [URI]
searchResultUris = map uri . map fst . lrResult . crRes

defaultContextSchema :: ContextSchema
defaultContextSchema = ContextSchema Nothing [] 1 True ctText

defaultContextInfo :: (Context, ContextSchema)
defaultContextInfo = ("default", defaultContextSchema)

insertDefaultContext :: Command
insertDefaultContext = uncurry InsertContext defaultContextInfo

insertTextContext :: Context -> Command
insertTextContext cx = InsertContext cx defaultContextSchema

-- ----------------------------------------------------------------------------

-- evaluate CM and check the result
testCM' :: Bool -> TestCM () -> Assertion
testCM' b int = do
  env <- initHunt :: IO DefHuntEnv
  res <- runHunt int env
  (if b then isRight else isLeft) res @? "unexpected interpreter result: " ++ show res


-- evaluate CM and check if it yields a result
-- allows for a whole sequence of commands with tests inbetween
-- the interpreter can fail prematurely
testCM :: TestCM () -> Assertion
testCM = testCM' True

-- ----------------------------------------------------------------------------

-- fancy functions
-- characters were chosen without any reason
(@@@) :: Command -> (CmdResult -> IO b) -> TestCM b
a @@@ f = execCmd a >>= liftIO . f

(@@=) :: Command -> CmdResult -> TestCM ()
a @@= b = a @@@ (@?=b)

-- ----------------------------------------------------------------------------

test_ranking :: Assertion
test_ranking = testCM $ do
  insertTextContext "0"
    @@= ResOK

  Insert brainDoc
    @@= ResOK

  Search (QWord QNoCase brain) os pp
    @@@ ((@?= [(brainUri, 1.0)]) . sr)

  Search (QBoost 2.0 (QWord QNoCase brain)) os pp
    @@@ ((@?= [(brainUri, 2.0)]) . sr)

  Search (QBinary And (QWord QNoCase brain) (QWord QNoCase brain)) os pp
    @@@ ((@?= [(brainUri, 1.0)]) . sr)

  Search (QBinary And (QBoost 4.0 (QWord QNoCase brain)) (QBoost 8.0 (QWord QNoCase brain))) os pp
    @@@ ((@?= [(brainUri, 6.0)]) . sr)

  -- no average calculation
  Search (QBinary And (QBoost 5.0 (QWord QNoCase brain)) (QWord QNoCase brain)) os pp
    @@@ ((@?= [(brainUri, 5.0)]) . sr)

  Search (QBinary And (QBoost 5.0 (QWord QNoCase brain)) (QBoost 1.0 (QWord QNoCase brain))) os pp
    @@@ ((@?= [(brainUri, 3.0)]) . sr)

  Delete brainUri
    @@= ResOK

  insertTextContext "1"
    @@= ResOK

  Insert pinkyDoc
    @@= ResOK

  Search (QWord QNoCase pinky) os pp
    @@@ ((@?= [(pinkyUri, 2.0)]) . sr)

  Search (QBoost 2.0 (QWord QNoCase pinky)) os pp
    @@@ ((@?= [(pinkyUri, 4.0)]) . sr)

  Search (QBinary And (QWord QNoCase pinky) (QWord QNoCase pinky)) os pp
    @@@ ((@?= [(pinkyUri, 2.0)]) . sr)

  Search (QBinary And (QBoost 4.0 (QWord QNoCase pinky)) (QBoost 8.0 (QWord QNoCase pinky))) os pp
    @@@ ((@?= [(pinkyUri, 12.0)]) . sr)

  -- no average calculation for default/implicit boosts
  Search (QBinary And (QBoost 4.0 (QWord QNoCase pinky)) (QWord QNoCase pinky)) os pp
    @@@ ((@?= [(pinkyUri, 8.0)]) . sr)

  Search (QBinary And (QBoost 5.0 (QWord QNoCase pinky)) (QBoost 1.0 (QWord QNoCase pinky))) os pp
    @@@ ((@?= [(pinkyUri, 6.0)]) . sr)


  where
  os = 0
  pp = 1000
  sr = map (first uri) . lrResult . crRes

  descr = M.empty

  brain    = "brain"
  brainUri = "test://brain"
  brainDoc :: ApiDocument
  brainDoc = emptyApiDoc
    { adUri   = brainUri
    , adIndex = M.fromList [("0", brain)]
    , adDescr = descr
    }

  pinky    = "pinky"
  pinkyUri = "test://pinky"
  pinkyDoc :: ApiDocument
  pinkyDoc = emptyApiDoc
    { adUri   = pinkyUri
    , adIndex = M.fromList [("0", pinky), ("1", pinky)]
    , adDescr = descr
    }
