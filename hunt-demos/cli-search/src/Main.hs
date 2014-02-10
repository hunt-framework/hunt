{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Data.Aeson                     (toJSON, eitherDecode)
import           Data.Aeson.Encode.Pretty
import           Data.Text                      (Text, unpack)
import           Data.Map                       hiding (map)
import qualified Data.ByteString.Lazy           as B

import           Hunt.Common
import           Hunt.Index.Schema
import           Hunt.Interpreter.Interpreter
import           Hunt.Interpreter.Command
import           Hunt.Query.Ranking
import           Hunt.Query.Language.Grammar
import           Hunt.Query.Language.Parser

import           System.Console.CmdArgs (cmdArgs, (&=), explicit, name, help, summary)
import           Data.Typeable (Typeable)
import           Data.Data (Data)

data CliConfig = CliConfig {
    cfgIndex :: String
} deriving (Show, Data, Typeable)


mkConfig :: CliConfig
mkConfig = CliConfig {
        cfgIndex = "index"  &= name "index" &= help "Path to a file containing a binary serialized hunt index."
    } &= summary "Hunt command line search"

main :: IO ()  
main = do
    config <- cmdArgs mkConfig
    ix <- initEnv emptyIndexer defaultRankConfig contextTypes
    putStrLn $ "loading index from file " ++ cfgIndex config
    res <- runCmd ix $ LoadIx (cfgIndex config)
    _ <-case res of 
         (Right _)  -> return ()
         (Left err) -> error $ show err
    processQuery ix
    return ()


processQuery ix = do
  putStrLn "Enter query:"
  qry <- getLine
  if qry == "exit" 
    then return()
    else do
      case parseQuery qry of
        (Right query) -> do
           putStrLn "searching ..."
           result <- runCmd ix $ Search query 0 10
           print result
           return ()
        (Left err)   -> putStrLn $ "Invalid input: " ++ unpack err
      processQuery ix
