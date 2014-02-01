{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Applicative
import           Network.HTTP
import           Test.QuickCheck
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Test.QuickCheck.Monadic as QM

import           Text.Lorem.Words
import           Criterion.Main

main :: IO ()
main = defaultMain $ do 
  [ bench "test" ( quickCheck simple_query) ]
  
loremText :: Gen Text
loremText = do
  words <- listOf1 $ elements latin
  return $ T.intercalate " " words

word :: Gen Text
word = elements latin


-- takes random word from lorem ipsum dictionary and runs query
simple_query :: Property
simple_query = QM.monadicIO $ do
                          w <- QM.pick word
                          passed <- QM.run $ query (T.unpack w)
                          QM.assert True

-- runs http query on holumbus server
query  :: String -> IO ()
query x = do
  rsp <- Network.HTTP.simpleHTTP (getRequest $ "http://localhost:3000/search/" ++ x)
  return ()
--  case rsp of
--    (Right r) -> do
--      print $ rspBody r
--    _ -> return ()



