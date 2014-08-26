module Hunt.Index.Helper where

import           Data.List                      (intersect)

import           Hunt.Common.DocId              (DocId, mkDocId)
import           Hunt.Common.Occurrences
import           Hunt.Common.IntermediateValue

-- ----------------------------------------------------------------------------
-- `Index` test helpers

docId1 :: DocId
docId1 = mkDocId (1::Int)

docId2 :: DocId
docId2 = mkDocId (2::Int)

fromDocId :: DocId -> IntermediateValue
fromDocId docId = toIntermediate $ singleton docId 1

simpleValue :: Int -> IntermediateValue
simpleValue i = toIntermediate $ singleton (mkDocId i) i

simpleValue1 :: IntermediateValue
simpleValue1 = simpleValue 1

simpleValue2 :: IntermediateValue
simpleValue2 = simpleValue 2

simpleValue1b :: IntermediateValue
simpleValue1b = complexValue 1 2

complexValue :: Int -> Int -> IntermediateValue
complexValue id' pos = toIntermediate $ singleton (mkDocId id') pos

checkResult :: Monad m => [IntermediateValue] -> [(x, IntermediateValue)] -> m Bool
checkResult vs res = return $ vs == (vs `intersect` map snd res)

addKey :: x -> [IntermediateValue] -> [(x, IntermediateValue)]
addKey key = map (\v -> (key, v))



