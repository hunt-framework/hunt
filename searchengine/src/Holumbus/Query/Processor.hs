{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE ConstraintKinds   #-}

-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Processor
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable

  The Holumbus query processor. Supports exact word or phrase queries as well
  as fuzzy word and case-insensitive word and phrase queries. Boolean
  operators like AND, OR and NOT are supported. Context specifiers and
  priorities are supported, too.

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Processor
  (
  -- * Processor types
  ProcessConfig (..)

  -- * Processing
  --, processQuery
  , processPartial
  , processQueryM
  , processPartialM

  -- * ?
  , getFuzzyConfigM
  , setContextsM
  , initStateM
  , forAllContextsM
)

where

import           Control.Applicative
import           Control.Monad
import           Control.Parallel.Strategies

import           Data.Binary                       (Binary (..))
import           Data.Function
import qualified Data.List                         as L
import qualified Data.Map                          as M
import           Data.Maybe
import           Data.Text                         (Text)
import qualified Data.Text                         as T
import           Data.Text.Lazy                    (fromStrict, toStrict)
import qualified Data.Text.Lazy.Read               as TR
import           Data.Text.Lazy.Builder            (toLazyText)
import qualified Data.Text.Lazy.Builder.Int        as TBI (decimal)

import           Holumbus.Utility

import           Holumbus.Common
import qualified Holumbus.Common.DocIdMap          as DM
import qualified Holumbus.Common.Positions         as Pos
import           Holumbus.Index.TextIndex

import           Holumbus.Index.Proxy.ContextIndex (ContextIndex)
import qualified Holumbus.Index.Proxy.ContextIndex as CIx

import           Holumbus.Query.Fuzzy              (FuzzyConfig, FuzzyScore)
import qualified Holumbus.Query.Fuzzy              as F
import           Holumbus.Query.Intermediate       (Intermediate)
import qualified Holumbus.Query.Intermediate       as I
import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Result             (Result)

import           Holumbus.DocTable.DocTable        (DocTable)
import qualified Holumbus.DocTable.DocTable        as Dt

import           Holumbus.Analyzer.Analyzer

-- ----------------------------------------------------------------------------

-- | The configuration for the query processor.
data ProcessConfig
    = ProcessConfig
      { fuzzyConfig   :: ! FuzzyConfig -- ^ The configuration for fuzzy queries.
      , optimizeQuery :: ! Bool        -- ^ Optimize the query before processing.
      , wordLimit     :: ! Int         -- ^ The maximum number of words used from a prefix. Zero switches off limiting.
      , docLimit      :: ! Int         -- ^ The maximum number of documents taken into account. Zero switches off limiting.
      }

instance Binary ProcessConfig where
  put (ProcessConfig fc o l d)
      = put fc >> put o >> put l >> put d
  get
      = liftM4 ProcessConfig get get get get

-- | The internal state of the query processor.
data ProcessState i
    = ProcessState
      { config   :: ! ProcessConfig    -- ^ The configuration for the query processor.
      , contexts :: ! [Context]        -- ^ The current list of contexts.
      -- XXX: strictness annotation
      , index    ::   ContextIndex i Occurrences  -- ^ The index to search.
      , schema   ::   ContextSchema
      , total    :: ! Int              -- ^ The number of documents in the index.
      }

-- |  shortcuts
type QueryIndex    i = ContextIndex     i Occurrences
type QueryIndexCon i = ContextTextIndex i Occurrences

-- ----------------------------------------------------------------------------

-- TODO: transform all monadic functions and delete this section

processPartialM :: (Monad m, QueryIndexCon i) => ProcessConfig -> QueryIndex i -> ContextSchema -> Int -> Query -> m Intermediate
processPartialM cfg i s t q = return $ processPartial cfg i s t q

-- ----------------------------------------------------------------------------

-- | Get the fuzzy config out of the process state.
getFuzzyConfig :: ProcessState i -> FuzzyConfig
getFuzzyConfig = fuzzyConfig . config

-- | Monadic version of 'getFuzzyConfig'.
getFuzzyConfigM :: Monad m => ProcessState i -> m FuzzyConfig
getFuzzyConfigM s = return $ fuzzyConfig $ config s

-- | Set the current context in the state.
setContexts :: [Context] -> ProcessState i -> ProcessState i
setContexts cs (ProcessState cfg _ i s t) = ProcessState cfg cs i s t

-- | Monadic version of 'setContexts'.
setContextsM :: Monad m => [Context] -> ProcessState i -> m (ProcessState i)
setContextsM cs (ProcessState cfg _ i s t) = return $ ProcessState cfg cs i s t

-- | Initialize the state of the processor.
initState :: (TextIndex i v) => ProcessConfig -> QueryIndex i -> ContextSchema -> Int -> ProcessState i
initState cfg i s = ProcessState cfg wcs i s
  where
  -- TODO: default context weights should be used here
  -- should be stored in interpreter schema and then
  -- somehow used here.
  wcs = CIx.contexts i

-- | Monadic version of 'initState'.
initStateM :: (Monad m, QueryIndexCon i) => ProcessConfig -> QueryIndex i -> ContextSchema -> Int -> m (ProcessState i)
initStateM cfg i s t = contextsM i >>= \cs -> return $ ProcessState cfg cs i s t
  where
  contextsM = return . CIx.contexts

-- TODO: previously rdeepseq
-- | Try to evaluate the query for all contexts in parallel.
forAllContexts :: (Context -> Intermediate) -> [Context] -> Intermediate
forAllContexts f wcs = I.unions $ parMap rseq f wcs

-- | Monadic version of 'forAllContexts'.
forAllContextsM :: Monad m => (Context -> m Intermediate) -> [Context] -> m Intermediate
forAllContextsM f cs = mapM f cs >>= \is -> return $ I.unions is

-- | Just everything.
allDocuments :: QueryIndexCon i => ProcessState i -> Intermediate
allDocuments s = forAllContexts (\c -> I.fromList "" c $ ixSize (index s) c) (contexts s)
  where
  -- FIXME: Ix.size replacement -- size :: i -> Context -> RawResult
  --        - this solution requires a (Just c, Nothing) case for the ContextIndex lookup function
  --          and the type param will be ignored
  --        - ixSize i c = concat . map snd . filter ((== c) . fst) . Ix.toList $ i
  --          this should be slower
  ixSize i c = toRawResult $ CIx.searchWithCx PrefixNoCase c "" i

{-
allDocumentsM :: (Monad m, TextIndex i v) => ProcessState i -> m Intermediate
allDocumentsM s = forAllContextsM (\c -> allWordsM (index s) c >>= \r -> return $ I.fromList "" c r) (contexts s)
  where allWordsM = return .:: Ix.size -- XXX: needs monadic Ix.allWords
-}

-- | Process a query only partially in terms of a distributed index. Only the intermediate
-- result will be returned.
processPartial :: QueryIndexCon i => ProcessConfig -> QueryIndex i -> ContextSchema -> Int -> Query -> Intermediate
processPartial cfg i s t q = process (initState cfg i s t) oq
  where
  oq = if optimizeQuery cfg then optimize q else q

{-
-- | Monadic version of 'processPartial'.
processPartialM :: (Monad m, TextIndex i v) => ProcessConfig -> QueryIndex i -> Int -> Query -> m Intermediate
processPartialM cfg i t q = initStateM cfg i t >>= flip processM oq
  where
  oq = if optimizeQuery cfg then optimize q else q
-}

-- XXX: DocTable dependency

-- | Process a query on a specific index with regard to the configuration.
--processQuery :: (QueryIndexCon i, DocTable d, Dt.DValue d ~ e, e ~  Document) =>
--                ProcessConfig -> QueryIndex i -> d -> Query -> Result e
--processQuery cfg i d q = I.toResult d (processPartial cfg i (Dt.size d) q)

-- | Monadic version of 'processQuery'.
processQueryM :: (Applicative m, Monad m, QueryIndexCon i, DocTable d, Dt.DValue d ~ e, e ~ Document) =>
                 ProcessConfig -> QueryIndex i -> ContextSchema -> d -> Query -> m (Result e)
processQueryM cfg i s d q = do
    sz <- Dt.size d
    processPartialM cfg i s sz q >>= \ir -> I.toResult d ir

-- | Continue processing a query by deciding what to do depending on the current query element.
process :: QueryIndexCon i => ProcessState i -> Query -> Intermediate
-- word search
process s (QWord QCase w)     = processCaseWord s w
process s (QWord QNoCase w)   = processWord s w
process s (QWord QFuzzy w)    = processFuzzyWord s w
-- phrase search
process s (QPhrase QCase w)   = processPhrase s Case w
process s (QPhrase QNoCase w) = processPhrase s NoCase w
process s (QPhrase QFuzzy w)  = processPhrase s Fuzzy w
process s (QNegation q)       = processNegation s (process s q)
process s (QContext c q)      = process (setContexts c s) q
process s (QBinary o q1 q2)   = processBin o (process s q1) (process s q2)
process s (QRange l h)        = processRange s l h
-- TODO: implement boosting of queries: (QBoost w q)

{--- | Monadic version of 'process'.
processM :: (Monad m, TextIndex i v) => ProcessState i -> Query -> m Intermediate
processM s (Word w)           = processWordM s w
processM s (Phrase w)         = processPhraseM s w
processM s (CaseWord w)       = processCaseWordM s w
processM s (CasePhrase w)     = processCasePhraseM s w
processM s (FuzzyWord w)      = processFuzzyWordM s w
processM s (Negation q)       = processM s q >>= processNegationM s
processM s (Specifier c q)    = setContextsM c s >>= \ns -> processM ns q
processM s (QBinary o q1 q2)  = do
  ir1 <- processM s q1
  ir2 <- processM s q2
  return $ processBin o ir1 ir2
-}

-- ----------------------------------------------------------------------------
-- Range Query
-- ----------------------------------------------------------------------------

-- TODO: error handling
processRange :: QueryIndexCon i => ProcessState i -> Text -> Text -> Intermediate
processRange s l h
  = let res = map contextSensitiveRange (contexts s)
  in if any isNothing res then error "range query not compatible with context types"
     else process s $ chainQueries1 . catMaybes $ res
  where
  -- FIXME: constructing a single query probably requires the Query to be fully evaluated beforehand
  -- (especially when using query optimization) -
  -- this leads to the string range to be fully evaluated which requires a lot of memory
  contextSensitiveRange :: Context -> Maybe Query
  contextSensitiveRange c
    = do
     (cType, rex, cNormalizer, _cWeight) <- contextSchema c
     let scan w = unbox .  scanTextRE rex . normalize cNormalizer $ w
     let validRange = (<=) -- TODO: context-sensitive check
     l' <- scan l
     h' <- scan h
     guard $ validRange l' h'
     return $ QContext [c] $ rangeQueryMapping cType l' h'

  contextSchema :: Context -> Maybe ContextType
  contextSchema c = M.lookup c $ schema s

  unbox [e] = Just e
  unbox _   = Nothing


-- range queries will be transformed to other queries for now
rangeQueryMapping :: CType -> Text -> Text -> Query
rangeQueryMapping t = case t of
  CText -> createTextRangeQuery
  CInt  -> createIntRangeQuery

createTextRangeQuery :: Text -> Text -> Query
createTextRangeQuery l h = naiveRangeQuery1 $ rangeText l h

-- NOTE: range limits have to be readable as Ints and the range has to contain at least 2 elements.
createIntRangeQuery :: Text -> Text -> Query
createIntRangeQuery l h = naiveRangeQuery1 . map show' $ [l'..h']
  where
  -- XXX: custom reader?
  read' = fst . fromRight . TR.decimal . fromStrict
  show' = toStrict . toLazyText . TBI.decimal
  _limits :: (Int, Int)
  _limits@(l',h') = (read' l, read' h)

-- NOTE: list must be non-empty.
naiveRangeQuery1 :: [Text] -> Query
naiveRangeQuery1 = chainQueries1 . map (QWord QCase)

-- NOTE: list must be non-empty.
chainQueries1 :: [Query] -> Query
chainQueries1 = L.foldl1' (QBinary Or)

-- | 'rangeString' with 'Text'.
rangeText :: Text -> Text -> [Text]
rangeText l h = map T.pack $ rangeString (T.unpack l) (T.unpack h)

-- [str0..str1]
rangeString :: String -> String -> [String]
rangeString start0 end = rangeString' start0
  where
  len = length end
  rangeString' start
    = let succ' = succString len start
    in if start == end
       then [start]
       else start:rangeString' succ'

{-
-- tail-recursive version
-- NOTE: order is reversed.
rangeString' :: String -> String -> [String]
rangeString' start0 end = range [] start0
  where
  len = length end
  range acc start
    = let succ' = succString len start
      in if start == end
         then start:acc
         else range (start:acc) succ'
-}

-- | Computes the string successor with a length restriction.
--   NOTE: characters need to be in range of minBound'..maxBound'.
succString :: Int -> String -> String
succString = succString'
  where
  succString' :: Int -> String -> String
  succString' m s =
    if length s < m
    then s ++ [minBound']
    else
      if null s
      then s
      else
        if l == maxBound'
        then succString' (m-1) i
        else i ++ [succ l]
    where
    (i,l) = (init s, last s)
    -- TODO: appropriate bounds?
    --minBound' = 'a'
    --maxBound' = 'z'
    minBound' = '!'
    maxBound' = '~'

-- ----------------------------------------------------------------------------
-- Word Query
-- ----------------------------------------------------------------------------

-- | Process a single, case-insensitive word by finding all documents whreturn I.empty -- ich contain the word as prefix.
processWord :: QueryIndexCon i => ProcessState i -> Text -> Intermediate
processWord s q = forAllContexts wordNoCase (contexts s)
  where
  wordNoCase c = I.fromList q c $ limitWords s . toRawResult $ CIx.searchWithCx PrefixNoCase c q (index s)

{-
-- | Monadic version of 'processWord'.
processWordM :: (Monad m, TextIndex i v) => ProcessState i -> Text -> m Intermediate
processWordM s q = forAllContextsM wordNoCase (contexts s)
  where
  wordNoCase c = prefixNoCaseM (index s) c q >>= limitWordsM s >>= \r -> return $ I.fromList q c r
  --prefixNoCaseM :: (Monad m, TextIndex i v) => i v -> Context -> Text -> m RawResult
  prefixNoCaseM i c w = return . toRawResult $ CIx.lookup PrefixNoCase i (Just c, Just w)  -- XXX: real monadic version
-}

-- | Process a single, case-sensitive word by finding all documents which contain the word as prefix.
processCaseWord :: QueryIndexCon i => ProcessState i -> Text -> Intermediate
processCaseWord s q = forAllContexts wordCase (contexts s)
  where
  wordCase c = I.fromList q c $ limitWords s . toRawResult $ CIx.searchWithCx PrefixCase c q (index s)

{-
-- | Monadic version of 'processCaseWord'.
processCaseWordM :: (Monad m, TextIndex i v) => ProcessState i -> Text -> m Intermediate
processCaseWordM s q = forAllContextsM wordCase (contexts s)
  where
  wordCase c = prefixCaseM (index s) c q >>= limitWordsM s >>= \r -> return $ I.fromList q c r
  prefixCaseM :: (Monad m, TextIndex i v) => i v -> Context -> Text -> m RawResult
  prefixCaseM i c w = return .::: CIx.lookup PrefixCase (Just c, Just w) i -- XXX: real monadic version
-}

-- | Process a single word and try some fuzzy alternatives if nothing was found.
processFuzzyWord :: QueryIndexCon i => ProcessState i -> Text -> Intermediate
processFuzzyWord s oq = processFuzzyWord' (F.toList $ F.fuzz (getFuzzyConfig s) oq) (processWord s oq)
  where
  processFuzzyWord' :: [(Text, FuzzyScore)] -> Intermediate -> Intermediate
  processFuzzyWord' []     r = r
  processFuzzyWord' (q:qs) r = if I.null r then processFuzzyWord' qs (processWord s (fst q)) else r

{-
-- | Monadic version of 'processFuzzyWord'.
processFuzzyWordM :: (Monad m, TextIndex i v) => ProcessState i -> Text -> m Intermediate
processFuzzyWordM s oq = do
  sr <- processWordM s oq
  cfg <- getFuzzyConfigM s
  processFuzzyWordM' (F.toList $ F.fuzz cfg oq) sr
    where
    processFuzzyWordM' []     r = return r
    processFuzzyWordM' (q:qs) r = if I.null r
                                  then processWordM s (fst q) >>= processFuzzyWordM' qs
                                  else return r
-}

-- ----------------------------------------------------------------------------
-- Phrase Query
-- ----------------------------------------------------------------------------

-- | Process a phrase.
processPhrase :: QueryIndexCon i => ProcessState i -> TextSearchOp -> Text -> Intermediate
processPhrase s op q = forAllContexts phraseNoCase (contexts s)
  where
  phraseNoCase c = processPhraseInternal meaningfulName c q
    where
    meaningfulName t = toRawResult $ CIx.searchWithCx op c t (index s)

{-
-- | Monadic version of 'processPhrase.
processPhraseM :: (Monad m, TextIndex i v) => ProcessState i -> Text -> m Intermediate
processPhraseM s q = forAllContextsM phraseNoCase (contexts s)
  where
  phraseNoCase c = processPhraseInternalM (CIx.lookup NoCase (index s) c) c q
-}

{-
-- | Monadic version of 'processCasePhrase'.
processCasePhraseM :: (Monad m, QueryIndexCon i) => ProcessState i -> Text -> m Intermediate
processCasePhraseM s q = forAllContextsM phraseCase (contexts s)
  where
  phraseCase c = processPhraseInternalM (Ix.lookup Case (index s) c) c q
-}

-- | Process a phrase query by searching for every word of the phrase and comparing their positions.
processPhraseInternal :: (Text -> RawResult) -> Context -> Text -> Intermediate
processPhraseInternal f c q = let
  (w:ws) = T.words q
  m = mergeOccurrencesList $ map snd $ f w in
  if DM.null m
  then I.empty
  else I.fromList q c [(q, processPhrase' ws 1 m)]
  where
  processPhrase' :: [Text] -> Position -> Occurrences -> Occurrences
  processPhrase' [] _ o = o
  processPhrase' (x:xs) p o = processPhrase' xs (p+1) (DM.filterWithKey (nextWord $ map snd $ f x) o)
    where
      nextWord :: [Occurrences] -> DocId -> Positions -> Bool
      nextWord [] _ _  = False
      nextWord no d np = maybe False hasSuccessor $ DM.lookup d (mergeOccurrencesList no)
          where
            hasSuccessor :: Positions -> Bool
            hasSuccessor w = Pos.foldr (\cp r -> r || Pos.member (cp + p) w) False np

{-
-- | Monadic version of 'processPhraseInternal'.
processPhraseInternalM :: Monad m => (Text -> RawResult) -> Context -> Text -> m Intermediate
processPhraseInternalM f c q = let
  w = T.words q
  m = mergeOccurrencesList $ map snd $ f (head w) in
  if DM.null m
  then return I.empty
  else do
    pp <- processPhrase' (tail w) 1 m
    return $ I.fromList q c [(q, pp)]
  where
  processPhrase' :: Monad m => [Text] -> Position -> Occurrences -> m Occurrences
  processPhrase' [] _ o = return o
  processPhrase' (x:xs) p o = processPhrase' xs (p+1) (DM.filterWithKey (nextWord . map snd $ f x) o)
    where
      nextWord :: [Occurrences] -> DocId -> Positions -> Bool
      nextWord [] _ _  = False
      nextWord no d np = maybe False hasSuccessor (DM.lookup d (mergeOccurrencesList no))
          where
            hasSuccessor :: Positions -> Bool
            hasSuccessor w = foldPos (\cp r -> r || memberPos (cp + p) w) False np
-}

-- ----------------------------------------------------------------------------
-- Operators
-- ----------------------------------------------------------------------------

-- | Process a negation by getting all documents and subtracting the result of the negated query.
processNegation :: QueryIndexCon i => ProcessState i -> Intermediate -> Intermediate
processNegation s = I.difference (allDocuments s)

{-
-- | Monadic version of 'processNegation'.
processNegationM :: (Monad m, TextIndex i v) => ProcessState i -> Intermediate -> m Intermediate
processNegationM s r1 = allDocumentsM s >>= \r2 -> return $ I.difference r2 r1
-}

-- | Process a binary operator by caculating the union or the intersection of the two subqueries.
processBin :: BinOp -> Intermediate -> Intermediate -> Intermediate
processBin And = I.intersection
processBin Or  = I.union
processBin But = I.difference

-- ----------------------------------------------------------------------------
-- Helper
-- ----------------------------------------------------------------------------

-- | Limit a 'RawResult' to a fixed amount of the best words.
--
-- First heuristic applied is limiting the number of documents in the result,
-- assuming the short words come first in the result list
-- So the length of the result list depends on the number of documents found.
--
-- TODO: This is fixed to 2000, should be part of the config part of the state
--
-- A 2. simple heuristic is used to
-- determine the quality of a word: The total number of occurrences divided by the number of
-- documents in which the word appears.
--
-- The second heuristic isn't that expensive any more when the resul list is cut of by the heuristic
--
-- The limit 500 should be part of a configuration
limitWords              :: ProcessState i -> RawResult -> RawResult
limitWords s r          = cutW . cutD $ r
  where
  limitD                = docLimit $ config s
  cutD
      | limitD > 0      = limitDocs limitD
      | otherwise       = id

  limitW                = wordLimit $ config s
  cutW
      | limitW > 0
        &&
        length r > limitW
                        = map snd . take limitW . L.sortBy (compare `on` fst) . map calcScore
      | otherwise       = id

  calcScore             :: (Word, Occurrences) -> (Double, (Word, Occurrences))
  calcScore w@(_, o)    = (log (fromIntegral (total s) / fromIntegral (DM.size o)), w)

-- ----------------------------------------------------------------------------

-- | Limit the number of docs in a raw result
limitDocs               :: Int -> RawResult -> RawResult
limitDocs _     []      = []
limitDocs limit _
    | limit <= 0        = []
limitDocs limit (x:xs)  = x : limitDocs (limit - DM.size (snd x)) xs

-- ----------------------------------------------------------------------------
{-
-- | Monadic version of 'limitWords'.
limitWordsM             :: (Monad m) => ProcessState i -> RawResult -> m RawResult
limitWordsM s r         = return $ limitWords s r
-}

-- | Merge occurrences
mergeOccurrencesList    :: [Occurrences] -> Occurrences
mergeOccurrencesList    = DM.unionsWith Pos.union

-- ----------------------------------------------------------------------------

-- XXX: no merging - just for results with a single context
toRawResult :: [(Context, [(Word, Occurrences)])] -> RawResult
toRawResult = concatMap snd

-- ----------------------------------------------------------------------------
