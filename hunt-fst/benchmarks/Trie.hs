module Main where

import qualified Hunt.FST as FST
import qualified Hunt.FST.Trie as Trie

import           Control.Applicative
import           Control.Arrow
import           Control.DeepSeq
import           Criterion
import           Criterion.Main
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as ByteString8
import qualified Data.List as List
import qualified Data.StringMap as SM
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import           Data.Word

loadWords :: IO ([(Text, Text)], [(Text, Text)])
loadWords
  = do big   <- Text.lines <$> Text.readFile "benchmarks/big.txt"
       small <- Text.lines <$> Text.readFile "benchmarks/small.txt"
       let mkPairs = fmap (\x -> (,) x x)
           cleanup = fmap Text.strip . filter (not . Text.null)

       return $!! (mkPairs $ small, mkPairs $ big)

loadWordsSM :: IO ([(String, Text)], ([(String, Text)]))
loadWordsSM
  = do big   <- Text.lines <$> Text.readFile "benchmarks/big.txt"
       small <- Text.lines <$> Text.readFile "benchmarks/small.txt"
       let mkPairs = fmap (\x -> (,) x x)
           toKey   = fmap (first Text.unpack)
       return $!! (toKey . mkPairs $ small, toKey . mkPairs $ big)

main :: IO ()
main
  = defaultMain [ env loadWordsSM $ \ ~(smWords, bgWords) ->
                   bgroup "stringmap" [ bench "fromList (small)" $ nf SM.fromList smWords
                                      , bench "fromList (big)"   $ nf SM.fromList bgWords
                                      ]
                , env loadWords $ \ ~(smWords, bgWords) ->
                   bgroup "hs-fst/trie" [ bench "compileTrie (small)" $ nf (Trie.output . snd . FST.compileListTrie) smWords
                                        , bench "compileTrie (big)"   $ nf (Trie.output . snd . FST.compileListTrie) bgWords
                                        ]
                ]
