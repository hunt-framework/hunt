{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad

import           Data.Aeson.Encode.Pretty
import           Data.Text                      (unpack)
import qualified Data.ByteString.Lazy           as B

import           Hunt.Query.Language.Parser

-- ----------------------------------------------------------------------------

main :: IO ()
main = do
  putStrLn "Enter 'exit' to quit."
  putStrLn "Enter query in textual syntax to parse it to json syntax"
  parseInput

parseInput :: IO ()
parseInput = do
  putStrLn ""
  qry <- getLine
  unless (qry == "exit") $ do
    case parseQuery qry of
      Right json -> B.putStr . encodePretty $ json
      Left err   -> putStrLn $ "Invalid input: " ++ unpack err
    parseInput
