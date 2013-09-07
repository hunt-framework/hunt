module ExampleServer where

import           Control.Concurrent.MVar
import           Control.Monad.Error
import           Control.Monad.Reader

import           Data.Aeson

-- ----------------------------------------------------------------------------

data Dummy
    = Dummy
      deriving (Show)

-- ----------------------------------------------------------------------------
--
-- the abstract syntax (syntactic domains)

type URI         = String
type Description = Dummy
type RawIndex    = Dummy

data RawDocument
    = Raw { _theUri      :: URI
          , _theDescr    :: Description
          , _theRawIndex :: RawIndex
          }
      deriving (Show)

data Query
    = QWord   String
    | QPrefix String
    | QAnd    Query Query
    | MoreQueryVariants
      deriving (Show)

data InsOpts
    = New | Replace | Modifiy
      deriving (Show)

data Command
    = Search     { _theQuery    :: Query }
    | Completion { _thePrefix   :: String }
    | Insert     { _theRawDoc   :: RawDocument
                 , _theInsOpts  :: InsOpts
                 }
    | Delete     { _theDocUri   :: URI }
    | LoadIx     { _thePath     :: FilePath }
    | StoreIx    { _thePath     :: FilePath }
    | Sequence   { _theCmdSeq   :: [Command] }
    | NOOP
    | MoreCommands
      deriving (Show)

data CmdRes
    = ResError { _theErr    :: Int
               , _theReason :: String
               }
    | ResCompl { _theWords  :: [String] }
    | ResOK
    | MoreResults
      deriving (Show)

instance Error CmdRes where
    strMsg s = ResError 500 $ "internal server error: " ++ s

instance FromJSON Command     where parseJSON = undefined
instance FromJSON InsOpts     where parseJSON = undefined
instance FromJSON Query       where parseJSON = undefined
instance FromJSON RawDocument where parseJSON = undefined

instance ToJSON Command       where toJSON = undefined
instance ToJSON CmdRes        where toJSON = undefined

-- ----------------------------------------------------------------------------
--
-- the semantic domains (datatypes for interpretation)
--
-- Env, Index, ...

-- ----------------------------------------------------------------------------
--
-- the environment
-- with a MVar for storing the index
-- so the MVar acts as a global state (within IO)

type Index      = Dummy
type Options    = Dummy
type OtherStuff = Dummy

data Env = Env
    { _theIndex      :: MVar Index
    , _theOptions    :: Options
    , _theOtherStuff :: OtherStuff
    }

emptyIndex      :: Index
emptyIndex      = Dummy

emptyOptions    :: Options
emptyOptions    = Dummy

emptyStuff      :: OtherStuff
emptyStuff      = Dummy

initEnv :: Index -> Options -> OtherStuff -> IO Env
initEnv ix opt os
    = do ixref <- newMVar ix
         return $ Env ixref opt os

-- ----------------------------------------------------------------------------
--
-- the command evaluation monad

type CM = ReaderT Env (ErrorT CmdRes IO)

runCmd :: Env -> Command -> IO CmdRes
runCmd env cmd
    = do res <- runErrorT $ runReaderT (execCmd cmd) env
         return $ either id id res

askIx :: CM Index
askIx
    = do ref <- asks _theIndex
         liftIO $ readMVar ref

withIx :: (Index -> CM a) -> CM a
withIx f
    = askIx >>= f

throwResError :: Int -> String -> CM a
throwResError n msg
    = throwError $ ResError n msg

-- ----------------------------------------------------------------------------

execCmd :: Command -> CM CmdRes
execCmd (Completion px)
    = withIx $ execCompletion px

execCmd (Search q)
    = withIx $ execSearch q

execCmd (Sequence cs)
    = execSequence cs

execCmd NOOP
    = return ResOK		-- keep alive test

execCmd c
    = throwResError 500 $ "command not yet implemented: " ++ show c

-- ----------------------------------------------------------------------------

execCompletion :: String -> Index -> CM CmdRes
execCompletion px _ix
    = return $ ResError 007 ("looking for completions for " ++ show px)

execSearch :: Query -> Index -> CM CmdRes
execSearch q _ix
    = return $ ResError 007 ("searching for: " ++ show q)

execSequence :: [Command] -> CM CmdRes
execSequence []       = execCmd NOOP
execSequence [c]      = execCmd c
execSequence (c : cs) = execCmd c >> execSequence cs

-- ----------------------------------------------------------------------------

main1 :: Command -> IO ()
main1 c
    = do env0 <- initEnv emptyIndex emptyOptions emptyStuff
         let eval = runCmd env0
         eval c >>= print
         return ()

-- ----------------------------------------------------------------------------

c1, c2, c3, c4, c5 :: Command
c1 = NOOP
c2 = Search (QWord "abc")
c3 = LoadIx  "ix1"
c4 = StoreIx "ix2"
c5 = Sequence [c1,c2,c3,c4]
