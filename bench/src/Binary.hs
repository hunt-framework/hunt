module Main where

import           Prelude                     as P

import           System.Environment

import           Control.Monad

import           Data.Aeson
import qualified Data.Binary                 as Bin
import qualified Data.ByteString.Lazy        as BL

import           Hunt.Common.ApiDocument

-- ----------------------------------------------------------------------------

getJson :: (FromJSON a) => FilePath -> IO a
getJson file = do
  content <- BL.readFile file
  return . fromLeft . eitherDecode $ content
  where
  fromLeft (Left msg) = error msg
  fromLeft (Right x)  = x

-- ----------------------------------------------------------------------------

-- type selector
type JsonType = ApiDocuments

-- ----------------------------------------------------------------------------

-- XXX: hack
dropExt :: FilePath -> FilePath
dropExt path
  = if '/' `elem` rExt
    then path
    else reverse $ tail' rBase
  where
  (rExt,rBase) = break (=='.') $ reverse path
  tail' xs = if null xs then xs else tail xs

-- ----------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  files <- getArgs
  forM_ files $ \file -> (getJson file :: IO JsonType) >>= Bin.encodeFile (dropExt file ++ ".bin")
