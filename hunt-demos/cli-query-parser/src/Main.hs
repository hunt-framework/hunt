{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Aeson                     (toJSON, eitherDecode)
import           Data.Aeson.Encode.Pretty
import           Data.Text                      (Text, unpack)
import           Data.Map                       hiding (map)
import qualified Data.ByteString.Lazy           as B

import           Hunt.Common
import           Hunt.Query.Language.Grammar
import           Hunt.Query.Language.Parser

main :: IO ()
main = do
  putStrLn "Enter 'exit' to quit."
  putStrLn "Enter query in textual syntax to parse it to json syntax"
  parseInput

parseInput :: IO ()
parseInput = do
  putStrLn ""
  qry <- getLine
  if qry == "exit" 
    then return()
    else do
      case parseQuery qry of
        (Right json) -> B.putStr . encodePretty $ json
        (Left err)   -> putStrLn $ "Invalid input: " ++ unpack err
      parseInput

