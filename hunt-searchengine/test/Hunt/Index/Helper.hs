module Hunt.Index.Helper where

import           Data.List                      (intersect)

import           Hunt.Common.DocId              (DocId, mkDocId)
import           Hunt.Common.Occurrences
import           Hunt.Scoring.SearchResult
-- import           Hunt.Common.IntermediateValue

-- ----------------------------------------------------------------------------
-- `Index` test helpers

docId1 :: DocId
docId1 = mkDocId (1::Int)

docId2 :: DocId
docId2 = mkDocId (2::Int)

fromDocId :: DocId -> SearchResult
fromDocId docId = mkSRfromOccurrences $ singleton docId 1

simpleValue :: Int -> SearchResult
simpleValue i = mkSRfromOccurrences $ singleton (mkDocId i) i

simpleValue1 :: SearchResult
simpleValue1 = simpleValue 1

simpleValue2 :: SearchResult
simpleValue2 = simpleValue 2

simpleValue1b :: SearchResult
simpleValue1b = complexValue 1 2

complexValue :: Int -> Int -> SearchResult
complexValue id' pos = mkSRfromOccurrences $ singleton (mkDocId id') pos

complexValues :: SearchResult
complexValues = mkSRfromOccurrences $
                merges [ singleton docId1 1
                       , singleton docId1 2
                       , singleton docId2 10
                       ]

checkResult :: Monad m => [SearchResult] -> [(x, SearchResult)] -> m Bool
checkResult vs res = return $ vs == (vs `intersect` map snd res)

addKey :: x -> [SearchResult] -> [(x, SearchResult)]
addKey key = map (\v -> (key, v))
