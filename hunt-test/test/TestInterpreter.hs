{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Map                      (Map)
import qualified Data.Map                      as M
import           Data.Text                     (Text)
import           Hunt.Common.Document
import qualified Hunt.Index.Proxy.ContextIndex as Ix
import           Hunt.Interpreter

-- ----------------------------------------------------------------------------

main1 :: Command -> IO ()
main1 c
    = do env0 <- initEnv emptyIndexer emptyOptions
         let eval = runCmd env0
         eval c >>= print
         return ()

-- ----------------------------------------------------------------------------

c1 = NOOP
c2 = Search  "abc"
c3 = LoadIx  "ix1"
c4 = StoreIx "ix2"
c5 = Sequence [c1,c2,c3,c4]

-- -----------------------------------------------------------------------------
--
type Position = Int
type Context = Text
type Word = Text
--
-- -- | Positions of Words for each context.
type Words = Map Context WordList
--
-- -- | Positions of words in the document.
type WordList = Map Word [Position]

mkWordList :: WordList
mkWordList = M.fromList $ [("word", [1,5,10])]

mkWords :: Words
mkWords = M.fromList $ [("context", mkWordList)]

mkDoc :: Document
mkDoc = Document "id::1" (M.fromList [("name", "Chris"), ("alter", "30")])

insertCmd = Insert mkDoc mkWords
searchCmd = Search "w"
batchCmd = Sequence [insertCmd,searchCmd]


-- --------------------------------------------------------------------------------
-- test add words

--test2 = test1 mkWords 1

--test1 :: Words -> Int -> ContextIndex InvertedIndex Occurrences -> ContextIndex InvertedIndex Occurrences
--test1 = addWords mkWords 1 Ix.empty
