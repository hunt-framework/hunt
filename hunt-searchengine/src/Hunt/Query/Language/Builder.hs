
module Hunt.Query.Language.Builder (

    qWord
    , qWordNoCase
    , qFullWord
    , qFullWordNoCase
    , qPhrase
    , qPhraseNoCase
    , qPrefixPhrase
    , qPrefixPhraseNoCase
    , qRange
    , qAnd
    , qAnds
    , qOr
    , qOrs
    , qAndNot
    , qAndNots
    , qNext
    , qNexts
    , qFollow
    , qFollows
    , qNear
    , qNears
    , setNoCaseSearch
    , setFuzzySearch
    , setContext
    , setContexts
    , setBoost
    , withinContexts    -- deprecated
    , withinContext     -- deprecated
    , withBoost         -- deprecated
    , qContext
) where

import           Hunt.Query.Language.Grammar
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Hunt.Common.BasicTypes      (Context, Weight)

-- query construction

-- | prefix search of a single word

qWord :: Text -> Query
qWord = QWord QCase

qWordNoCase :: Text -> Query
qWordNoCase = QWord QNoCase

-- | exact case sensitive search of a single word

qFullWord :: Text -> Query
qFullWord = QFullWord QCase

-- | exact, but case insensitive search of a single word

qFullWordNoCase :: Text -> Query
qFullWordNoCase = QFullWord QNoCase

-- --------------------
--
-- phrase search

qPhrase' :: (Text -> Query) -> Text -> Query
qPhrase' qf t
    = case T.words t of
        [w] -> qf w
        ws  -> qNexts $ map qf ws

-- | exact search of a sequence of space separated words.
-- For each word in the sequence, an exact word search is performed.

qPhrase :: Text -> Query
qPhrase = qPhrase' qFullWord

-- | exact, but case insenitive search of a sequence of space separated words.
-- For each word in the sequence, a word search is performed.

qPhraseNoCase :: Text -> Query
qPhraseNoCase = qPhrase' qFullWordNoCase

-- | prefix search of a sequence of space separated words.
-- For each word in the sequence, a prefix search is performed.

qPrefixPhrase :: Text -> Query
qPrefixPhrase = qPhrase' qWordNoCase

-- | prefix search of a sequence of space separated words.
-- For each word in the sequence, a prefix search is performed.

qPrefixPhraseNoCase :: Text -> Query
qPrefixPhraseNoCase = qPhrase' qWordNoCase

-- --------------------

-- | search a range of words or an intervall for numeric contexts
qRange :: Text -> Text -> Query
qRange = QRange

-- | shortcut for case sensitive context search
qContext :: Context -> Text -> Query
qContext c w = QContext [c] $ QWord QCase w

-- | and query
qAnd :: Query -> Query -> Query
qAnd q1 q2 = qAnds [q1, q2]

--  | multiple @and@ queries. The list must not be emtpy
qAnds :: [Query] -> Query
qAnds = mkAssocSeq And

-- | or query
qOr :: Query -> Query -> Query
qOr q1 q2 = qOrs [q1, q2]

--  | multiple @or@ queries. The list must not be emtpy
qOrs :: [Query] -> Query
qOrs = mkAssocSeq Or

-- | and not query
qAndNot :: Query -> Query -> Query
qAndNot q1 q2 = qAndNots [q1, q2]

--  | multiple @and-not@ queries. The list must not be emtpy
-- TODO handle left associativity

qAndNots :: [Query] -> Query
qAndNots = mkLeftAssocSeq AndNot

-- | neighborhood queries. The list must not be empty
--
-- TODO: a better name for qNext and qNexts, qPhrase is already used

qNext :: Query -> Query -> Query
qNext q1 q2 = qNexts [q1, q2]

qNexts :: [Query] -> Query
qNexts = mkAssocSeq Phrase

qFollow :: Int -> Query -> Query -> Query
qFollow d q1 q2 = qFollows d [q1, q2]

qFollows :: Int -> [Query] -> Query
qFollows d = mkAssocSeq (Follow d)

qNear :: Int -> Query -> Query -> Query
qNear d q1 q2 = qNears d [q1, q2]

qNears :: Int -> [Query] -> Query
qNears d = mkAssocSeq (Near d)

collectAssocs :: BinOp -> [Query] -> [Query]
collectAssocs op qs
    = concatMap subqs qs
    where
      subqs (QSeq op' qs')
          | op == op'
              = qs'
      subqs q'
          = [q']

mkAssocSeq :: BinOp -> [Query] -> Query
mkAssocSeq op qs
    = remSingle $ QSeq op (collectAssocs op qs)

mkLeftAssocSeq :: BinOp -> [Query] -> Query
mkLeftAssocSeq op qs
    = remSingle $ QSeq op qs'
    where
      qs' = case qs of
              (QSeq op' qs1 : qs2)
                  | op == op'
                      -> qs1 ++ qs2
              _       -> qs

remSingle :: Query -> Query
remSingle (QSeq _ [q])
    = q
remSingle q
    = q


-- ------------------------------------------------------------
-- configure simple search queries

-- | case insensitve search, only sensible for word and phrase queries

setNoCaseSearch :: Query -> Query
setNoCaseSearch (QWord     _ w) = QWord     QNoCase w
setNoCaseSearch (QFullWord _ w) = QFullWord QNoCase w
setNoCaseSearch (QPhrase   _ w) = QPhrase   QNoCase w
setNoCaseSearch q             = q

-- | fuzzy search, only sensible for word and phrase queries

setFuzzySearch :: Query -> Query
setFuzzySearch (QWord     _ w) = QWord     QFuzzy w
setFuzzySearch (QFullWord _ w) = QFullWord QFuzzy w
setFuzzySearch (QPhrase   _ w) = QPhrase   QFuzzy w
setFuzzySearch q             = q

-- | restrict search to list of contexts

setContexts :: [Context] -> Query -> Query
setContexts = QContext

withinContexts :: [Context] -> Query -> Query
withinContexts = QContext
{-# DEPRECATED withinContexts "Don't use this, use setContexts" #-}

-- | restrict search to a single context

setContext :: Context -> Query -> Query
setContext cx = withinContexts [cx]

withinContext :: Context -> Query -> Query
withinContext cx = setContexts [cx]
{-# DEPRECATED withinContext "Don't use this, use setContext" #-}


-- | boost the search results by a factor

setBoost :: Weight -> Query -> Query
setBoost = QBoost

withBoost :: Weight -> Query -> Query
withBoost = QBoost
{-# DEPRECATED withBoost "Don't use this, use setBoost" #-}    
