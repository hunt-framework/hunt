module Hunt.RankingTests
(rankingTests)
where

import           Control.Monad.Except
import qualified Data.Map                       as M
import           Data.Text                      (Text)
import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit                     hiding (Test)
import           Hunt.Common
import qualified Hunt.Common.DocDesc            as DD
import           Hunt.Common.Document
import           Hunt.ClientInterface
import           Hunt.DocTable.HashedDocTable   (Documents)
import           Hunt.Interpreter
import           Hunt.Query.Ranking
import           Hunt.Utility

-- ----------------------------------------------------------------------------

type TestEnv  = HuntEnv (Documents Document)
type TestCM a = Hunt    (Documents Document) a

rankConfig :: DocumentWrapper e => RankConfig e
rankConfig = defaultRankConfig

-- XXX split this up into separate test cases...
rankingTests :: [Test]
rankingTests =
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
searchResultUris = map uri . lrResult . crRes

defaultContextSchema :: ContextSchema
defaultContextSchema = ContextSchema Nothing [] 1 True ctText

defaultContextInfo :: (Context, ContextSchema)
defaultContextInfo = ("default", defaultContextSchema)

insertDefaultContext :: Command
insertDefaultContext = uncurry cmdInsertContext defaultContextInfo

insertTextContext :: Context -> Command
insertTextContext cx = cmdInsertContext cx defaultContextSchema

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

search :: Query -> Int -> Int -> Command
search q o m = setResultOffset o . setMaxResults m . cmdSearch $ q

test_ranking :: Assertion
test_ranking = testCM $ do
  insertTextContext "0"
    @@= ResOK

  cmdInsertDoc brainDoc
    @@= ResOK

  search (qWordNoCase brain) os pp
    @@@ ((@?= [(brainUri, 1.0)]) . sr)

  search (setBoost 2.0 (qWordNoCase brain)) os pp
    @@@ ((@?= [(brainUri, 2.0)]) . sr)

  search (qAnd (qWordNoCase brain) (qWordNoCase brain)) os pp
    @@@ ((@?= [(brainUri, 1.0)]) . sr)

  search (qAnd (setBoost 4.0 (qWordNoCase brain)) (setBoost 8.0 (qWordNoCase brain))) os pp
    @@@ ((@?= [(brainUri, 6.0)]) . sr)

  -- no average calculation
  search (qAnd (setBoost 5.0 (qWordNoCase brain)) (qWordNoCase brain)) os pp
    @@@ ((@?= [(brainUri, 5.0)]) . sr)

  search (qAnd (setBoost 5.0 (qWordNoCase brain)) (setBoost 1.0 (qWordNoCase brain))) os pp
    @@@ ((@?= [(brainUri, 3.0)]) . sr)

  cmdDeleteDoc brainUri
    @@= ResOK

  insertTextContext "1"
    @@= ResOK

  cmdInsertDoc pinkyDoc
    @@= ResOK

  search (qWordNoCase pinky) os pp
    @@@ ((@?= [(pinkyUri, 2.0)]) . sr)

  search (setBoost 2.0 (qWordNoCase pinky)) os pp
    @@@ ((@?= [(pinkyUri, 4.0)]) . sr)

  search (qAnd (qWordNoCase pinky) (qWordNoCase pinky)) os pp
    @@@ ((@?= [(pinkyUri, 2.0)]) . sr)

  search (qAnd (setBoost 4.0 (qWordNoCase pinky)) (setBoost 8.0 (qWordNoCase pinky))) os pp
    @@@ ((@?= [(pinkyUri, 12.0)]) . sr)

  -- no average calculation for default/implicit boosts
  search (qAnd (setBoost 4.0 (qWordNoCase pinky)) (qWordNoCase pinky)) os pp
    @@@ ((@?= [(pinkyUri, 8.0)]) . sr)

  search (qAnd (setBoost 5.0 (qWordNoCase pinky)) (setBoost 1.0 (qWordNoCase pinky))) os pp
    @@@ ((@?= [(pinkyUri, 6.0)]) . sr)


  where
  os = 0
  pp = 1000
  sr = map (\x -> (uri x, score x)) . lrResult . crRes

  descr = DD.empty

  brain    = "brain"
  brainUri = "test://brain"
  brainDoc :: ApiDocument
  brainDoc
      = setIndex (M.fromList [("0", brain)])
        $ setDescription descr
        $ mkApiDoc brainUri

  pinky    = "pinky"
  pinkyUri = "test://pinky"
  pinkyDoc :: ApiDocument
  pinkyDoc
      = setIndex (M.fromList [("0", pinky), ("1", pinky)])
        $ setDescription descr
        $ mkApiDoc pinkyUri
