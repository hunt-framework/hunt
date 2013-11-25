{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
   processQuery
,  initState

,  ProcessConfig (..)
,  ProcessState
)

where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State

import qualified Data.Binary                       as Bin
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

import           Holumbus.Query.Fuzzy              (FuzzyConfig)
import qualified Holumbus.Query.Fuzzy              as F
import           Holumbus.Query.Intermediate       (Intermediate)
import qualified Holumbus.Query.Intermediate       as I
import           Holumbus.Query.Language.Grammar
import           Holumbus.Query.Result             (Result)

import           Holumbus.DocTable.DocTable        (DocTable)
import qualified Holumbus.DocTable.DocTable        as Dt

import           Holumbus.Analyzer.Analyzer
import           Holumbus.Analyzer.Normalizer
import           Holumbus.Interpreter.Command      (CmdError(..))

-- ----------------------------------------------------------------------------
-- | The configuration and State for the query processor.
-- ----------------------------------------------------------------------------

data ProcessConfig
    = ProcessConfig
      -- ^ The configuration for fuzzy queries.
      { fuzzyConfig   :: ! FuzzyConfig
      -- ^ Optimize the query before processing.
      , optimizeQuery :: ! Bool
      -- ^ The maximum number of words used from a prefix. Zero switches off limiting.
      , wordLimit     :: ! Int
      -- ^ The maximum number of documents taken into account. Zero switches off limiting.
      , docLimit      :: ! Int
}

instance Bin.Binary ProcessConfig where
  put (ProcessConfig fc o l d)
      = Bin.put fc >> Bin.put o >> Bin.put l >> Bin.put d
  get
      = liftM4 ProcessConfig Bin.get Bin.get Bin.get Bin.get

-- | The internal state of the query processor.
data ProcessState i
    = ProcessState
      { config   :: ! ProcessConfig    -- ^ The configuration for the query processor.
      , contexts :: ! [Context]        -- ^ The current list of contexts.
      , index    ::   ContextIndex i Occurrences  -- ^ The index to search.
      , schema   ::   Schema           -- ^ Schema / Schemas for the contexts.
      , total    :: ! Int              -- ^ The number of documents in the index.
      }

-- ----------------------------------------------------------------------------
-- | Processor monad
-- ----------------------------------------------------------------------------

type QueryIndex    i = ContextIndex     i Occurrences
type QueryIndexCon i = ContextTextIndex i Occurrences

-- | the processor monad
newtype ProcessorT ix m a = PT { runProcessor :: StateT (ProcessState ix) (ErrorT CmdError m) a }
  deriving (Applicative, Monad, MonadIO, Functor, MonadState (ProcessState ix), MonadError CmdError)

instance MonadTrans (ProcessorT ix) where
  lift = PT . lift . lift

type Processor ix = ProcessorT ix IO


-- ----------------------------------------------------------------------------
-- | helper
-- ----------------------------------------------------------------------------

processError :: QueryIndexCon ix => Int -> Text -> Processor ix a
processError n msg
    = throwError $ ResError n msg

getContexts :: QueryIndexCon ix => Processor ix [Context]
getContexts = get >>= return . contexts

getConfig :: QueryIndexCon ix => Processor ix ProcessConfig
getConfig = get >>= return . config

getFuzzyConfig :: QueryIndexCon ix => Processor ix FuzzyConfig
getFuzzyConfig = get >>= return . fuzzyConfig . config

getIx :: QueryIndexCon ix => Processor ix (QueryIndex ix)
getIx = get >>= return . index

getSchema :: QueryIndexCon ix => Processor ix Schema
getSchema = get >>= return . schema

withState' :: QueryIndexCon ix => (ProcessState ix -> Processor ix a) -> Processor ix a
withState' f = get >>= f

-- | Monadic version of 'setContexts'.
putContexts :: QueryIndexCon ix => [Context] -> Processor ix ()
putContexts cs = modify setCx
  where
  setCx (ProcessState cfg _ ix s dts) = ProcessState cfg cs ix s dts


-- | Initialize the state of the processor.
initState :: QueryIndexCon i
          => ProcessConfig -> QueryIndex i -> Schema -> Int
          -> ProcessState i
initState cfg ix s dtSize
  = ProcessState cfg cxs ix s dtSize
  where -- XXX: kind of inefficient
  cxs = filter (\c -> fromMaybe False $ M.lookup c s >>= return . cxDefault) $ CIx.contexts ix

-- ----------------------------------------------------------------------------
-- | processor code
-- ----------------------------------------------------------------------------

processQuery :: (QueryIndexCon i, DocTable d, Dt.DValue d ~ e, e ~ Document)
              => ProcessState i -> d -> Query -> IO (Either CmdError (Result e))
processQuery st d q = runErrorT . evalStateT (runProcessor processToRes) $ st
    where
    oq = if optimizeQuery (config st) then optimize q else q
    processToRes = process oq >>= \ir -> I.toResult d ir

process :: QueryIndexCon ix   => Query -> Processor ix Intermediate
process o = case o of
  QWord QCase w       -> forAllContexts . processWordCase      $ w
  QWord QNoCase w     -> forAllContexts . processWordNoCase    $ w
  QWord QFuzzy w      -> processFuzzyWord                      $ w
  QPhrase QCase w     -> forAllContexts' . processPhraseCase   $ w
  QPhrase QNoCase w   -> forAllContexts' . processPhraseNoCase $ w
  QPhrase QFuzzy w    -> forAllContexts' . processPhraseFuzzy  $ w
  QNegation q         -> process q >>= processNegation
  QContext c q        -> putContexts c >> process q
  QBinary op q1 q2    -> do -- XXX: maybe parallel
                          pq1 <- process q1
                          pq2 <- process q2
                          processBin op pq1 pq2
  QRange l h          -> processRange l h


-- TODO: previously rdeepseq
-- TODO: parallelize mapM
-- | Try to evaluate the query for all contexts in parallel.
--   version with implizit state
forAllContexts :: (QueryIndexCon i)
               => (Context -> ProcessState i -> Processor i Intermediate)
               -> Processor i Intermediate
forAllContexts f = getContexts >>= mapM (\c -> get >>= f c) >>= return . I.unions

-- | second version with explizit state in function
--   not sure which way is more elagant
forAllContexts' :: (QueryIndexCon i)
               => (Context -> Processor i Intermediate)
               -> Processor i Intermediate
forAllContexts' f = getContexts >>= mapM f >>= return . I.unions

-- ----------------------------------------------------------------------------
-- Word Query
-- ----------------------------------------------------------------------------

-- | Process a single, case-insensitive word by finding all documents
--   which contain the word as prefix.
processWord :: QueryIndexCon i
            => TextSearchOp -> Text -> Context -> ProcessState i
            -> Processor i Intermediate
processWord op q c st
    = return . I.fromList q c . limitWords st . toRawResult
    $ CIx.searchWithCx op c q (index st)

-- | Case Sensitive variant of process Word
processWordCase :: QueryIndexCon i
                => Text -> Context -> ProcessState i
                -> Processor i Intermediate
processWordCase = processWord PrefixCase

-- | Case Insensitive variant of process Word
processWordNoCase :: QueryIndexCon i
                  => Text -> Context -> ProcessState i
                  -> Processor i Intermediate
processWordNoCase = processWord PrefixNoCase

-- | Calculates a set of Fuzzy words and queries for all of them
--   NOTE: this might be very inefficient
--   XXX TODO: Optimize Performance
--               - mapConcurrent
--               - upper bound for size of fuzzyset
processFuzzyWord :: QueryIndexCon i => Text -> Processor i Intermediate
processFuzzyWord q = do
  cfg <- getFuzzyConfig
  is <- mapM (forAllContexts . processWordNoCase . fst) $ fuzzySet cfg
  return . I.unions $ is
  where
  fuzzySet cfg = (q,0):(F.toList $ F.fuzz cfg q)

-- ----------------------------------------------------------------------------
-- Phrase Query
-- ----------------------------------------------------------------------------

-- | Process a phrase.
processPhrase :: QueryIndexCon i
              => TextSearchOp
              -> Text -> Context
              -> Processor i Intermediate
processPhrase op q c = do
    ix <- getIx
    processPhraseInternal (meaningfulName ix) q c
    where
    meaningfulName ix t = toRawResult $ CIx.searchWithCx op c t ix

processPhraseCase   :: QueryIndexCon i => Text -> Context -> Processor i Intermediate
processPhraseCase   = processPhrase Case

processPhraseNoCase :: QueryIndexCon i => Text -> Context -> Processor i Intermediate
processPhraseNoCase = processPhrase NoCase

processPhraseFuzzy  :: QueryIndexCon i => Text -> Context -> Processor i Intermediate
processPhraseFuzzy  = processPhrase Fuzzy

-- | Process a phrase query by searching for every word of the phrase and comparing their positions.
processPhraseInternal :: QueryIndexCon i
                      => (Text -> RawResult) -> Text -> Context
                      -> Processor i Intermediate
processPhraseInternal f c q =
  if DM.null result
    then return $ I.empty
    else return $ I.fromList q c [(q, processPhrase' ws 1 result)]
  where
  result = mergeOccurrencesList $ map snd $ f w
  (w:ws) = T.words q
  processPhrase' :: [Text] -> Position -> Occurrences -> Occurrences
  processPhrase' [] _ o = o
  processPhrase' (x:xs) p o = processPhrase' xs (p+1) (DM.filterWithKey (nextWord $ map snd $ f x) o)
    where
      nextWord :: [Occurrences] -> DocId -> Positions -> Bool
      nextWord [] _ _  = False
      nextWord no d np = maybe False hasSuccessor $ DM.lookup d (mergeOccurrencesList no)
          where
            hasSuccessor :: Positions -> Bool
            hasSuccessor w' = Pos.foldr (\cp r -> r || Pos.member (cp + p) w') False np

-- ----------------------------------------------------------------------------
-- Range Query
-- ----------------------------------------------------------------------------
-- TODO: error handling
processRange :: QueryIndexCon i => Text -> Text -> Processor i Intermediate
processRange l h = do
  cs     <- getContexts
  s      <- getSchema
  let mqs = map (contextSensitiveRange s) cs
  if any isNothing mqs
    then processError 101 "range query not compatible with context types"
    else process $ chainQueries1 . catMaybes $ mqs
  where
  -- FIXME: constructing a single query probably requires the Query to be fully evaluated beforehand
  -- (especially when using query optimization) -
  -- this leads to the string range to be fully evaluated which requires a lot of memory
  contextSensitiveRange :: Schema -> Context -> Maybe Query
  contextSensitiveRange s c
    = do
     cSchema <- M.lookup c s
     let cType       = cxType cSchema
         rex         = cxRegEx cSchema
         cNormalizer = cxNormalizer cSchema
     guard $ all (typeValidator cType) [l,h]
     let scan w      = unbox . map (normalize cNormalizer) . scanTextRE rex $ w
         validRange  = (<=) -- TODO: context-sensitive check
     l' <- scan l
     h' <- scan h
     guard $ validRange l' h'
     return $ QContext [c] $ rangeQueryMapping cType l' h'

  unbox [e] = Just e
  unbox _   = Nothing


-- range queries will be transformed to other queries for now
rangeQueryMapping :: CType -> Text -> Text -> Query
rangeQueryMapping t = case t of
  CText -> createTextRangeQuery
  CInt  -> createIntRangeQuery
  CDate -> createDateRangeQuery -- XXX: same as for Text

createDateRangeQuery :: Text -> Text -> Query
createDateRangeQuery = createTextRangeQuery

createTextRangeQuery :: Text -> Text -> Query
createTextRangeQuery = naiveRangeQuery1 .:: rangeText

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
  succString' m s
    | length s < m   = s ++ [minBound']
    | null s         = s
    | l == maxBound' = succString' (m - 1) i
    | otherwise      = i ++ [succ l]
    where
    (i, l) = (init s, last s)
    minBound' = '!'
    maxBound' = '~'
    -- TODO: appropriate bounds?
    --minBound' = 'a'
    --maxBound' = 'z'

-- ----------------------------------------------------------------------------
-- Operators
-- ----------------------------------------------------------------------------

-- | Process a negation by getting all documents and subtracting the result of the negated query.
processNegation :: QueryIndexCon i => Intermediate -> Processor i Intermediate
processNegation i = allDocuments >>= \allDocs -> return $ I.difference allDocs i

-- | Process a binary operator by caculating the union or the intersection of the two subqueries.
processBin :: QueryIndexCon i
           => BinOp -> Intermediate -> Intermediate -> Processor i Intermediate
processBin And i1 i2 = return $ I.intersection i1 i2
processBin Or  i1 i2 = return $ I.union        i1 i2
processBin But i1 i2 = return $ I.difference   i1 i2


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

-- | Merge occurrences
mergeOccurrencesList    :: [Occurrences] -> Occurrences
mergeOccurrencesList    = DM.unionsWith Pos.union

-- ----------------------------------------------------------------------------

-- XXX: no merging - just for results with a single context
toRawResult :: [(Context, [(Word, Occurrences)])] -> RawResult
toRawResult = concatMap snd


-- | Just everything.
allDocuments :: QueryIndexCon i => Processor i Intermediate
allDocuments = forAllContexts (\c s -> return $ I.fromList "" c $ ixSize (index s) c)
  where
  ixSize i c = toRawResult $ CIx.searchWithCx PrefixNoCase c "" i
