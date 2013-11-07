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
import           Data.Text                         (Text)
import qualified Data.Text                         as T

--import           Holumbus.Utility                  ((.::), (.:::))

import           Holumbus.Common                   ( Context, DocId, Position,
                                                     RawResult, Textual (..),
                                                     Word, Positions, Document )
import qualified Holumbus.Common.DocIdMap          as DM
import           Holumbus.Common.Occurrences       as Occ
import           Holumbus.Common.Positions         (foldPos, memberPos, unionPos)
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
data ProcessState i v
    = ProcessState
      { config   :: ! ProcessConfig   -- ^ The configuration for the query processor.
      , contexts :: ! [Context]       -- ^ The current list of contexts.
      -- XXX: strictness annotation
      , index    ::   ContextIndex i v-- ^ The index to search.
      , total    :: ! Int             -- ^ The number of documents in the index.
      }

-- ----------------------------------------------------------------------------

-- TODO: transform all monadic functions and delete this section

processPartialM :: (Monad m, ContextTextIndex i v) => ProcessConfig -> ContextIndex i v -> Int -> Query -> m Intermediate
processPartialM cfg i t q = return $ processPartial cfg i t q

-- ----------------------------------------------------------------------------

-- | Get the fuzzy config out of the process state.
getFuzzyConfig :: ProcessState i v -> FuzzyConfig
getFuzzyConfig = fuzzyConfig . config

-- | Monadic version of 'getFuzzyConfig'.
getFuzzyConfigM :: Monad m => ProcessState i v -> m FuzzyConfig
getFuzzyConfigM s = return $ fuzzyConfig $ config s

-- | Set the current context in the state.
setContexts :: [Context] -> ProcessState i v -> ProcessState i v
setContexts cs (ProcessState cfg _ i t) = ProcessState cfg cs i t

-- | Monadic version of 'setContexts'.
setContextsM :: Monad m => [Context] -> ProcessState i v -> m (ProcessState i v)
setContextsM cs (ProcessState cfg _ i t) = return $ ProcessState cfg cs i t

-- | Initialize the state of the processor.
initState :: (TextIndex i v) => ProcessConfig -> ContextIndex i v -> Int -> ProcessState i v
initState cfg i = ProcessState cfg (CIx.keys i) i

-- | Monadic version of 'initState'.
initStateM :: (Monad m, ContextTextIndex i v) => ProcessConfig -> ContextIndex i v -> Int -> m (ProcessState i v)
initStateM cfg i t = contextsM i >>= \cs -> return $ ProcessState cfg cs i t
  where contextsM = return . CIx.keys -- XXX: needs monadic Ix.contexts

-- TODO: previously rdeepseq
-- | Try to evaluate the query for all contexts in parallel.
forAllContexts :: (Context -> Intermediate) -> [Context] -> Intermediate
forAllContexts f cs = L.foldl' I.union I.empty $ parMap rseq f cs

-- | Monadic version of 'forAllContexts'.
forAllContextsM :: Monad m => (Context -> m Intermediate) -> [Context] -> m Intermediate
forAllContextsM f cs = mapM f cs >>= \is -> return $ L.foldl' I.union I.empty is

-- | Just everything.
allDocuments :: ContextTextIndex i v => ProcessState i v -> Intermediate
allDocuments s = forAllContexts (\c -> I.fromList "" c $ ixSize (index s) c) (contexts s)
  where
  -- FIXME: Ix.size replacement -- size :: i -> Context -> RawResult
  --        - this solution requires a (Just c, Nothing) case for the ContextIndex lookup function
  --          and the type param will be ignored
  --        - ixSize i c = concat . map snd . filter ((== c) . fst) . Ix.toList $ i
  --          this should be slower
  ixSize i c = toRawResult $ CIx.lookup dummy (Just c, Nothing) i
  dummy = PrefixCase -- unused with this type of lookup

{-
allDocumentsM :: (Monad m, TextIndex i v) => ProcessState i v -> m Intermediate
allDocumentsM s = forAllContextsM (\c -> allWordsM (index s) c >>= \r -> return $ I.fromList "" c r) (contexts s)
  where allWordsM = return .:: Ix.size -- XXX: needs monadic Ix.allWords
-}

-- | Process a query only partially in terms of a distributed index. Only the intermediate
-- result will be returned.
processPartial :: ContextTextIndex i v => ProcessConfig -> ContextIndex i v -> Int -> Query -> Intermediate
processPartial cfg i t q = process (initState cfg i t) oq
  where
  oq = if optimizeQuery cfg then optimize q else q

{-
-- | Monadic version of 'processPartial'.
processPartialM :: (Monad m, TextIndex i v) => ProcessConfig -> ContextIndex i v -> Int -> Query -> m Intermediate
processPartialM cfg i t q = initStateM cfg i t >>= flip processM oq
  where
  oq = if optimizeQuery cfg then optimize q else q
-}

-- XXX: DocTable dependency

-- | Process a query on a specific index with regard to the configuration.
--processQuery :: (ContextTextIndex i v, DocTable d, Dt.DValue d ~ e, e ~  Document) =>
--                ProcessConfig -> ContextIndex i v -> d -> Query -> Result e
--processQuery cfg i d q = I.toResult d (processPartial cfg i (Dt.size d) q)

-- | Monadic version of 'processQuery'.
processQueryM :: (Applicative m, Monad m, ContextTextIndex i v, DocTable d, Dt.DValue d ~ e, e ~ Document) =>
                 ProcessConfig -> ContextIndex i v -> d -> Query -> m (Result e)
processQueryM cfg i d q = do
    sz <- Dt.size d
    processPartialM cfg i sz q >>= \ir -> I.toResult d ir

-- | Continue processing a query by deciding what to do depending on the current query element.
process :: ContextTextIndex i v => ProcessState i v -> Query -> Intermediate
process s (Word w)           = processWord s w
process s (Phrase w)         = processPhrase s w
process s (CaseWord w)       = processCaseWord s w
process s (CasePhrase w)     = processCasePhrase s w
process s (FuzzyWord w)      = processFuzzyWord s w
process s (Negation q)       = processNegation s (process s q)
process s (Specifier c q)    = process (setContexts c s) q
process s (BinQuery o q1 q2) = processBin o (process s q1) (process s q2)

{--- | Monadic version of 'process'.
processM :: (Monad m, TextIndex i v) => ProcessState i v -> Query -> m Intermediate
processM s (Word w)           = processWordM s w
processM s (Phrase w)         = processPhraseM s w
processM s (CaseWord w)       = processCaseWordM s w
processM s (CasePhrase w)     = processCasePhraseM s w
processM s (FuzzyWord w)      = processFuzzyWordM s w
processM s (Negation q)       = processM s q >>= processNegationM s
processM s (Specifier c q)    = setContextsM c s >>= \ns -> processM ns q
processM s (BinQuery o q1 q2) = do
  ir1 <- processM s q1
  ir2 <- processM s q2
  return $ processBin o ir1 ir2
-}

-- | Process a single, case-insensitive word by finding all documents whreturn I.empty -- ich contain the word as prefix.
processWord :: ContextTextIndex i v => ProcessState i v -> Text -> Intermediate
processWord s q = forAllContexts wordNoCase (contexts s)
  where
  wordNoCase c = I.fromList q c $ limitWords s . toRawResult $ CIx.lookup PrefixNoCase (Just c, Just q) (index s)

{-
-- | Monadic version of 'processWord'.
processWordM :: (Monad m, TextIndex i v) => ProcessState i v -> Text -> m Intermediate
processWordM s q = forAllContextsM wordNoCase (contexts s)
  where
  wordNoCase c = prefixNoCaseM (index s) c q >>= limitWordsM s >>= \r -> return $ I.fromList q c r
  --prefixNoCaseM :: (Monad m, TextIndex i v) => i v -> Context -> Text -> m RawResult
  prefixNoCaseM i c w = return . toRawResult $ CIx.lookup PrefixNoCase i (Just c, Just w)  -- XXX: real monadic version
-}

-- | Process a single, case-sensitive word by finding all documents which contain the word as prefix.
processCaseWord :: ContextTextIndex i v => ProcessState i v -> Text -> Intermediate
processCaseWord s q = forAllContexts wordCase (contexts s)
  where
  wordCase c = I.fromList q c $ limitWords s . toRawResult $ CIx.lookup PrefixCase (Just c, Just q) (index s)

{-
-- | Monadic version of 'processCaseWord'.
processCaseWordM :: (Monad m, TextIndex i v) => ProcessState i v -> Text -> m Intermediate
processCaseWordM s q = forAllContextsM wordCase (contexts s)
  where
  wordCase c = prefixCaseM (index s) c q >>= limitWordsM s >>= \r -> return $ I.fromList q c r
  prefixCaseM :: (Monad m, TextIndex i v) => i v -> Context -> Text -> m RawResult
  prefixCaseM i c w = return .::: CIx.lookup PrefixCase (Just c, Just w) i -- XXX: real monadic version
-}

-- | Process a phrase case-insensitive.
processPhrase :: ContextTextIndex i v => ProcessState i v -> Text -> Intermediate
processPhrase s q = forAllContexts phraseNoCase (contexts s)
  where
  phraseNoCase c = processPhraseInternal meaningfulName c q
    where
    meaningfulName t = toRawResult $ CIx.lookup NoCase (Just c, Just t) (index s)

{-
-- | Monadic version of 'processPhrase'.
processPhraseM :: (Monad m, TextIndex i v) => ProcessState i v -> Text -> m Intermediate
processPhraseM s q = forAllContextsM phraseNoCase (contexts s)
  where
  phraseNoCase c = processPhraseInternalM (CIx.lookup NoCase (index s) c) c q
-}

-- | Process a phrase case-sensitive.
processCasePhrase :: ContextTextIndex i v => ProcessState i v -> Text -> Intermediate
processCasePhrase s q = forAllContexts phraseCase (contexts s)
  where
  phraseCase c = processPhraseInternal meaningfulName c q
    where
    meaningfulName t = toRawResult $ CIx.lookup Case (Just c, Just t) (index s)

{-
-- | Monadic version of 'processCasePhrase'.
processCasePhraseM :: (Monad m, ContextTextIndex i v) => ProcessState i v -> Text -> m Intermediate
processCasePhraseM s q = forAllContextsM phraseCase (contexts s)
  where
  phraseCase c = processPhraseInternalM (Ix.lookup Case (index s) c) c q
-}

-- | Process a phrase query by searching for every word of the phrase and comparing their positions.
processPhraseInternal :: (Text -> RawResult) -> Context -> Text -> Intermediate
processPhraseInternal f c q = let
  w = T.words q
  m = mergeOccurrencesList $ map snd $ f (head w) in
  if DM.null m
  then I.empty
  else I.fromList q c [(q, processPhrase' (tail w) 1 m)]
  where
  processPhrase' :: [Text] -> Position -> Occurrences -> Occurrences
  processPhrase' [] _ o = o
  processPhrase' (x:xs) p o = processPhrase' xs (p+1) (DM.filterWithKey (nextWord $ map snd $ f x) o)
    where
      nextWord :: [Occurrences] -> DocId -> Positions -> Bool
      nextWord [] _ _  = False
      nextWord no d np = maybe False hasSuccessor (DM.lookup d (mergeOccurrencesList no))
          where
            hasSuccessor :: Positions -> Bool
            hasSuccessor w = foldPos (\cp r -> r || memberPos (cp + p) w) False np

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

-- | Process a single word and try some fuzzy alternatives if nothing was found.
processFuzzyWord :: ContextTextIndex i v => ProcessState i v -> Text -> Intermediate
processFuzzyWord s oq = processFuzzyWord' (F.toList $ F.fuzz (getFuzzyConfig s) oq) (processWord s oq)
  where
  processFuzzyWord' :: [(Text, FuzzyScore)] -> Intermediate -> Intermediate
  processFuzzyWord' []     r = r
  processFuzzyWord' (q:qs) r = if I.null r then processFuzzyWord' qs (processWord s (fst q)) else r

{-
-- | Monadic version of 'processFuzzyWord'.
processFuzzyWordM :: (Monad m, TextIndex i v) => ProcessState i v -> Text -> m Intermediate
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

-- | Process a negation by getting all documents and subtracting the result of the negated query.
processNegation :: ContextTextIndex i v => ProcessState i v -> Intermediate -> Intermediate
processNegation s = I.difference (allDocuments s)

{-
-- | Monadic version of 'processNegation'.
processNegationM :: (Monad m, TextIndex i v) => ProcessState i v -> Intermediate -> m Intermediate
processNegationM s r1 = allDocumentsM s >>= \r2 -> return $ I.difference r2 r1
-}

-- | Process a binary operator by caculating the union or the intersection of the two subqueries.
processBin :: BinOp -> Intermediate -> Intermediate -> Intermediate
processBin And r1 r2 = I.intersection r1 r2
processBin Or  r1 r2 = I.union        r1 r2
processBin But r1 r2 = I.difference   r1 r2


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
limitWords              :: ProcessState i v -> RawResult -> RawResult
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
limitWordsM             :: (Monad m) => ProcessState i v -> RawResult -> m RawResult
limitWordsM s r         = return $ limitWords s r
-}

-- | Merge occurrences
mergeOccurrencesList    :: [Occurrences] -> Occurrences
mergeOccurrencesList    = DM.unionsWith unionPos

-- ----------------------------------------------------------------------------

-- XXX: no merging - just for results with a single context
toRawResult :: [(Context, [(Word, Occurrences)])] -> RawResult
toRawResult = concat . map snd

-- ----------------------------------------------------------------------------
