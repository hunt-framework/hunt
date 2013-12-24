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
import           Control.Arrow                     (second)
import           Control.Monad
import           Control.Monad.Error
import           Control.Monad.State

import           Data.Binary                       (Binary)
import qualified Data.Binary                       as Bin
import           Data.Function
import qualified Data.List                         as L
import qualified Data.Map                          as M
import           Data.Maybe
import           Data.Text                         (Text)
import qualified Data.Text                         as T

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

import           Holumbus.Index.Schema.Analyze
import           Holumbus.Index.Schema.Normalize
import           Holumbus.Interpreter.Command      (CmdError(..))

import qualified System.Log.Logger                as Log

import           Holumbus.Utility.Log


-- | Name of the module for logging purposes.
modName :: String
modName = "Holumbus.Query.Processor"

-- | Log a message at 'DEBUG' priority.
debugM :: String -> IO ()
debugM = Log.debugM modName

-- | Log a message at 'WARNING' priority.
warningM :: String -> IO ()
warningM = Log.warningM modName

-- | Log a message at 'ERROR' priority.
errorM :: String -> IO ()
errorM = Log.errorM modName

-- ----------------------------------------------------------------------------
-- | The configuration and State for the query processor.
-- ----------------------------------------------------------------------------

data ProcessConfig
  = ProcessConfig
    { fuzzyConfig   :: ! FuzzyConfig -- ^ The configuration for fuzzy queries.
    , optimizeQuery :: ! Bool        -- ^ Optimize the query before processing.
    , wordLimit     :: ! Int         -- ^ The maximum number of words used from a prefix. @0@ = no limit.
    , docLimit      :: ! Int         -- ^ The maximum number of documents taken into account. @0@ = no limit.
    }

instance Binary ProcessConfig where
  put (ProcessConfig fc o l d)
    = Bin.put fc >> Bin.put o >> Bin.put l >> Bin.put d
  get
    = liftM4 ProcessConfig Bin.get Bin.get Bin.get Bin.get

-- | The internal state of the query processor.
data ProcessState i
  = ProcessState
    { psConfig   :: ! ProcessConfig    -- ^ The configuration for the query processor.
    , psContexts :: ! [Context]        -- ^ The current list of contexts.
    , psIndex    ::   ContextIndex i Occurrences  -- ^ The index to search.
    , psSchema   ::   Schema           -- ^ Schema / Schemas for the contexts.
    , psTotal    :: ! Int              -- ^ The number of documents in the index.
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

unless' :: QueryIndexCon i => Bool -> Int -> Text -> Processor i ()
unless' b code text = unless b $ processError code text

getContexts :: QueryIndexCon ix => Processor ix [Context]
getContexts = get >>= return . psContexts

--getConfig :: QueryIndexCon ix => Processor ix ProcessConfig
--getConfig = get >>= return . psConfig

getFuzzyConfig :: QueryIndexCon ix => Processor ix FuzzyConfig
getFuzzyConfig = get >>= return . fuzzyConfig . psConfig

getIx :: QueryIndexCon ix => Processor ix (QueryIndex ix)
getIx = get >>= return . psIndex

getSchema :: QueryIndexCon ix => Processor ix Schema
getSchema = get >>= return . psSchema

-- | Get the schema associated with that context/index.
--   /NOTE/: This fails if the schema does not exist.
getContextSchema :: QueryIndexCon ix => Context -> Processor ix ContextSchema
getContextSchema c = getSchema >>= return . fromJust . M.lookup c

-- | normalizes search text in respect of schema context type
--   first runs validator that throws error for invalid values
--   then runs normalizers attached to the given context
normQueryCx :: QueryIndexCon ix => Context -> Text -> Processor ix Text
normQueryCx c t = do
  s <- getContextSchema c
  if typeValidator (ct s) t 
    then do
       liftIO . debugM $ concat [ "query normalizer: ", T.unpack c, ": [", T.unpack t, "=>", T.unpack $ n s, "]"] 
       return $ n s 
    else processError 400 $ T.concat ["value incompatible with context type: ", c, ":", t, "(", T.pack . show $ ct s,")"]
  where
  n s = normalizeByType s t 
  ct s = cxType s

-- | normalizes search text in respect of multiple contexts
normQueryCxs :: QueryIndexCon ix => [Context] -> Text -> Processor ix [(Context, Text)]
normQueryCxs cs t  = mapM (\c -> normQueryCx c t >>= \nt -> return (c, nt)) cs

--withState' :: QueryIndexCon ix => (ProcessState ix -> Processor ix a) -> Processor ix a
--withState' f = get >>= f

-- | Set the contexts to be used for the query. Checks if the contexts exist.
putContexts :: QueryIndexCon ix => [Context] -> Processor ix ()
putContexts cs = do
  schema <- getSchema
  let invalidContexts = filter (not . flip M.member schema) cs
  if null invalidContexts
    then modify setCx
    else processError 404 $ "mentioned context(s) do not exist: " -- schema to be precise
                          `T.append` (T.pack . show $ invalidContexts)
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

processQuery :: (QueryIndexCon i, DocTable d, Dt.DValue d ~ e)
              => ProcessState i -> d -> Query -> IO (Either CmdError (Result e))
processQuery st d q = runErrorT . evalStateT (runProcessor processToRes) $ st
    where
    oq = if optimizeQuery (psConfig st) then optimize q else q
    processToRes = process oq >>= \ir -> I.toResult d ir

process :: QueryIndexCon ix   => Query -> Processor ix Intermediate
process o = case o of
  QWord QCase w       -> forAllContexts . processWordCase      $ w
  QWord QNoCase w     -> forAllContexts . processWordNoCase    $ w
  QWord QFuzzy w      -> processFuzzyWord                      $ w
  QPhrase QCase w     -> forAllContexts . processPhraseCase   $ w
  QPhrase QNoCase w   -> forAllContexts . processPhraseNoCase $ w
  QPhrase QFuzzy w    -> processPhraseFuzzy  $ w
  QContext c q        -> putContexts c >> process q
  QBinary op q1 q2    -> do -- XXX: maybe parallel
                          pq1 <- process q1
                          pq2 <- process q2
                          processBin op pq1 pq2
  QRange l h          -> processRange l h
  QBoost w q          -> process q >>= processBoost w


-- TODO: previously rdeepseq
-- TODO: parallelize mapM
-- | Evaluate (the query) for all contexts.
forAllContexts :: (QueryIndexCon i)
               => ([Context] -> Processor i Intermediate) -> Processor i Intermediate
forAllContexts f = getContexts >>= f

forAllContexts' :: (QueryIndexCon i)
                => (Context -> Processor i Intermediate) -> Processor i Intermediate
forAllContexts' f = getContexts >>= mapM f >>= return . I.unions


{-
-- version with explicit state
forAllContexts' :: (QueryIndexCon i)
-- | Try to evaluate the query for all contexts in parallel.
--   version with implizit state
               => (Context -> ProcessState i -> Processor i Intermediate)
               -> Processor i Intermediate
forAllContexts' f = getContexts >>= mapM (\c -> get >>= f c) >>= return . I.unions
-}

-- ----------------------------------------------------------------------------
-- Word Query
-- ----------------------------------------------------------------------------

-- | Process a single, case-insensitive word by finding all documents
--   which contain the word as prefix.
processWord :: QueryIndexCon i
            => TextSearchOp -> Text -> [Context]
            -> Processor i Intermediate
processWord op q c = do
  st <- get
  ix <- getIx
  cq <- normQueryCxs c q
  return . I.fromListCx q c . limitWords st . toRawResult
    $ CIx.searchWithCxsNormalized op cq ix

-- | Case Sensitive variant of process Word
processWordCase :: QueryIndexCon i
                => Text -> [Context]
                -> Processor i Intermediate
processWordCase = processWord PrefixCase

-- | Case Insensitive variant of process Word
processWordNoCase :: QueryIndexCon i
                  => Text -> [Context]
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
              -> Text -> [Context]
              -> Processor i Intermediate
processPhrase op q c = do
    ix <- getIx
    processPhraseInternal (meaningfulName ix) q c
    where
    meaningfulName ix t = toRawResult $ CIx.searchWithCxs op c t ix

processPhraseCase   :: QueryIndexCon i => Text -> [Context] -> Processor i Intermediate
processPhraseCase   = processPhrase Case

processPhraseNoCase :: QueryIndexCon i => Text -> [Context] -> Processor i Intermediate
processPhraseNoCase = processPhrase NoCase

processPhraseFuzzy  :: QueryIndexCon i => Text -> Processor i Intermediate
processPhraseFuzzy q = do
  cfg <- getFuzzyConfig
  is <- mapM (forAllContexts . processPhraseNoCase . fst) $ fuzzySet cfg
  return . I.unions $ is
  where
  fuzzySet cfg = (q,0):(F.toList $ F.fuzz cfg q)


-- | Process a phrase query by searching for every word of the phrase and comparing their positions.
processPhraseInternal :: QueryIndexCon i
                      => (Text -> RawResult) -> Text -> [Context]
                      -> Processor i Intermediate
processPhraseInternal f q c = do
  cq <- normQueryCxs c q
  if DM.null result
    then return $ I.empty
    else return $ I.fromListCx q c [(q, processPhrase' ws 1 result)]
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
processRange :: QueryIndexCon i => Text -> Text -> Processor i Intermediate
processRange l h = forAllContexts' range
  where
  range c = do
    cSchema <- getContextSchema c
    -- compatible with context
    let cType       = cxType   cSchema
        rangeText   = T.pack . show $ [l,h]
        scan        = scanTextRE (cxRegEx      cSchema)
        norm        = normalize  (cxNormalizer cSchema)
        rs@[ls, hs] = [scan l, scan h]
    -- all range values are valid
    unless' (all (typeValidator cType) $ concat rs)
            400 $ "range value(s) incompatible with context type: " `T.append` rangeText
    let (ls', hs') = (map norm $ ls, map norm $ hs)
    -- values form a valid range
    unless' (rangeValidator cType ls' hs')
            400 $ "invalid range for context: " `T.append` rangeText
    -- type determines the processing
    case cType of
      _ -> processRange' (unbox ls') (unbox hs') c

  processRange' :: QueryIndexCon i => Text -> Text -> Context -> Processor i Intermediate
  processRange' lo hi c = do
    st <- get
    ix <- getIx
    return . I.fromList lo c . limitWords st . toRawResult -- FIXME: check I.fromList
      $ CIx.lookupRange lo hi ix

{-
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
-}

-- ----------------------------------------------------------------------------
-- Operators
-- ----------------------------------------------------------------------------

-- | Process a binary operator by caculating the union or the intersection of the two subqueries.
processBin :: QueryIndexCon i
           => BinOp -> Intermediate -> Intermediate -> Processor i Intermediate
processBin And    i1 i2 = return $ I.intersection i1 i2
processBin Or     i1 i2 = return $ I.union        i1 i2
processBin AndNot i1 i2 = return $ I.difference   i1 i2


-- | Process query boosting
processBoost :: QueryIndexCon i => Float -> Intermediate -> Processor i Intermediate
processBoost b = return . DM.map (second (b:))

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
  limitD                = docLimit $ psConfig s
  cutD
      | limitD > 0      = limitDocs limitD
      | otherwise       = id

  limitW                = wordLimit $ psConfig s
  cutW
      | limitW > 0
        &&
        length r > limitW
                        = map snd . take limitW . L.sortBy (compare `on` fst) . map calcScore
      | otherwise       = id

  calcScore             :: (Word, Occurrences) -> (Double, (Word, Occurrences))
  calcScore w@(_, o)    = (log (fromIntegral (psTotal s) / fromIntegral (DM.size o)), w)

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
