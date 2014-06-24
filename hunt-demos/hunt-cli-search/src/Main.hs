{-# LANGUAGE DeriveDataTypeable #-}
module Main where

import           Control.Monad               (unless)
import           Data.Text                   (Text, unpack)
import           Data.Time.Clock

import           Hunt.ClientInterface
import           Hunt.Common
import           Hunt.Common.ApiDocument
import           Hunt.Index.Schema
import           Hunt.Interpreter
import           Hunt.Interpreter.Command
import           Hunt.Query.Language.Grammar
import           Hunt.Query.Language.Parser
import           Hunt.Query.Ranking
import           System.Environment

-- ----------------------------------------------------------------------------

-- | Hunt command line search.
--   After index is loaded, queries can be performed
--   'exit' stops the application
main :: IO ()
main = do
    args <- getArgs

    putStrLn $ "loading index from file " ++ head args
    ix  <- initHunt :: IO DefHuntEnv
    res <- runCmd ix $ cmdLoadIndex (head args)

    case res of
       Left err -> error $ show err
       _        -> getQuery ix

    where
    -- reads query from line
    getQuery ix = do
      putStrLn "======================================"
      putStrLn "Enter query or 'exit':"
      qry <- getLine
      unless (qry == "exit") $ do
        case parseQuery qry of
          Right query -> do
            putStrLn "searching ..."
            -- no paging here. fetch all results. might lead to slow queries!
            start <- getCurrentTime
            res <- runCmd ix $ cmdSearch query
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
            putStrLn $ "Raw result: " ++ show r
            mapM (\ Document {uri = u} -> putStr $ " " ++ unpack u) (lrResult . crRes $ r)
            putStrLn ""
            return ()
