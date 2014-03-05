{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Data.Text                      (Text, unpack)
import           Data.Time.Clock

import           Hunt.Common
import           Hunt.Common.ApiDocument
import           Hunt.Index.Schema
import           Hunt.Interpreter.Interpreter
import           Hunt.Interpreter.Command
import           Hunt.Query.Ranking
import           Hunt.Query.Language.Grammar
import           Hunt.Query.Language.Parser

import           System.Environment

-- | Hunt command line search.
--   After index is loaded, queries can be performed
--   'exit' stops the application
main :: IO ()
main = do
    args <- getArgs

    putStrLn $ "loading index from file " ++ head args
    ix  <- initHunt :: IO DefHuntEnv
    res <- runCmd ix $ LoadIx (head args)

    case res of
       Left err -> error $ show err
       _        -> getQuery ix

    where
    -- reads query from line
    getQuery ix = do
      putStrLn "======================================"
      putStrLn "Enter query or 'exit':"
      qry <- getLine
      if qry == "exit"
        then return ()
        else do
          case parseQuery qry of
            Right query -> do
              putStrLn "searching ..."
              -- no paging here. fetch all results. might lead to slow queries!
              start <- getCurrentTime
              res <- runCmd ix $ Search query 0 1000000000000
              end <- getCurrentTime
              printQueryResult res (diffUTCTime end start)
              getQuery ix
            Left err   -> putStrLn $ "Invalid input: " ++ unpack err

-- | Print interpreter search results.
printQueryResult :: Show t => Either CmdError CmdResult -> t -> IO ()
printQueryResult res t
    = case res of
        Left err-> do
            putStrLn $ "Query executing error:" ++ show err
        Right r -> do
            putStrLn "======================================"
            putStrLn $ "Query time: " ++ show t
            putStrLn $ "Query results: " ++ (show . lrCount . crRes $ r)
            putStr $ "Found uris: "
            mapM (\(Document uri _,_) -> putStr $ " " ++ unpack uri) (lrResult . crRes $ r)
            putStrLn ""
            return ()
