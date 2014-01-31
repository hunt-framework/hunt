{-# OPTIONS_GHC -fno-warn-orphans #-}
module Yesod.Hunt.SubSite where

import           Data.Text                        (Text)
import           Data.Aeson

import           Control.Applicative              ((<$>))

import           Hunt.Common

import qualified Hunt.Interpreter.Interpreter as Hol
import           Hunt.Interpreter.Command
import           Hunt.Query.Language.Parser

import           Yesod
import           Yesod.Hunt.Routes

-- |  some sort of json response format
data JsonResponse r = JsonSuccess r | JsonFailure Int [Text]

instance (ToJSON r) => ToJSON (JsonResponse r) where
  toJSON (JsonSuccess msg) = object
    [ "code"  .= (0 :: Int)
    , "msg"   .= msg
    ]

  toJSON (JsonFailure n msg) = object
    [ "code"  .= n
    , "msg"   .= msg
    ]

-- | A subsite needs to be an instance of YesodSubDispatch, which states how to
-- dispatch. By using constraints, we can make requirements of our master site.
instance YesodHolumbus master => YesodSubDispatch Holumbus (HandlerT master IO) where
    yesodSubDispatch = $(mkYesodSubDispatch resourcesHolumbus)


type HolHandler a = forall master. YesodHolumbus master
                  => HandlerT Holumbus (HandlerT master IO) a

-- | helper that runs command within holumbus index interpreter
runHolumbus :: Command -> HolHandler (Either CmdError CmdResult)
runHolumbus cmd = do
  env <- getHolumbus <$> getYesod
  res <- liftIO $ Hol.runCmd env cmd
  return res

-- | helper to run simple commands without results
runCmd :: Command -> HolHandler Value
runCmd cmd = do
  res <- runHolumbus cmd
  return $ case res of
    Right (ResOK)            -> toJSON $ JsonSuccess ("Ok"::Text)
    Left (ResError code msg) -> toJSON $ (JsonFailure code [msg] :: JsonResponse Text)
    _                        -> toJSON $ (JsonFailure 700 ["invalid operation"] :: JsonResponse Text)


-- | search for all documents
getHolSearch :: String -> HolHandler Value
getHolSearch query = getHolPagedSearch query 1 10000

-- | search for a subset of documents by page
getHolPagedSearch :: String -> Int -> Int -> HolHandler Value
getHolPagedSearch query p pp = do
  case parseQuery query of
    Left  err -> return $ toJSON $ (JsonFailure 700 [err] :: JsonResponse Text)
    Right qry -> do
      res <- runHolumbus $ Search qry p pp
      case res of
        Right (ResSearch docs)   -> return . toJSON
                                  $ JsonSuccess docs
        Left (ResError code msg) -> return . toJSON
                                  $ (JsonFailure code [msg] :: JsonResponse [Document])
        _                        -> return . toJSON
                                  $ (JsonFailure 700 ["invalid operation"] :: JsonResponse [Document])

-- | search for auto-completion terms
getHolCompletion :: String -> HolHandler Value
getHolCompletion query = do
  case parseQuery query of
    Left  err -> return . toJSON $ (JsonFailure 700 [err] :: JsonResponse Text)
    Right qry -> do
      res <- runHolumbus $ Completion qry
      case res of
        Right (ResCompletion ws) -> return . toJSON
                                  $ JsonSuccess ws
        Left (ResError code msg) -> return . toJSON
                                  $ (JsonFailure code [msg] :: JsonResponse [Text])
        _                        -> return . toJSON
                                  $ (JsonFailure 700 ["invalid operation"] :: JsonResponse [Text])

-- | insert document
postHolInsert :: HolHandler Value
postHolInsert = do
  docs <- parseJsonBody_ :: HolHandler [ApiDocument]
  let batch = Sequence $ map (flip Insert New) docs
  runCmd batch

-- | delete document
postHolDelete :: HolHandler Value
postHolDelete = do
  uris <- parseJsonBody_ :: HolHandler [URI]
  let batch = Sequence $ map Delete uris
  runCmd batch






