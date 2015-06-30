module Main where

import           Hunt.FST.Trie
import           Hunt.FST.Compile

import           Control.Applicative
import           Control.DeepSeq
import           Criterion
import           Criterion.Main
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

loadWords :: IO ([(Text, Text)], [(Text, Text)])
loadWords
  = do text <- Text.lines <$> Text.readFile "benchmarks/big.txt"
       let pairs = fmap (\x -> (,) x x) text
       return $!! (pairs, pairs)


main :: IO ()
main
  = defaultMain [ env loadWords $ \ ~(smWords, bgWords) ->
                    bgroup "trie" [ bench "compileList (big)" $ nf compileList' bgWords
                                  ]




                ]
