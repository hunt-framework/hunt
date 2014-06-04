{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- ----------------------------------------------------------------------------
{- |
  The query processor to perform 'Query's.

  'processQuery' executes the query and generates the unranked result.
  The result can be ranked with the default 'Hunt.Query.Ranking.rank' function.
-}
-- ----------------------------------------------------------------------------

module Hunt.Query.Processor
( processQuery
, processQueryDocIds , initProcessor

, ProcessConfig (..)
, ProcessEnv
)

where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad.Error
import           Control.Monad.Reader

import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Bin
import           Data.Default
import           Data.Function
import qualified Data.List                   as L
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Hunt.Common
import qualified Hunt.Common.DocIdMap        as DM
import qualified Hunt.Common.DocIdSet        as DS
import qualified Hunt.Common.Occurrences     as Occ
import qualified Hunt.Common.Positions       as Pos
import           Hunt.ContextIndex           (ContextMap)
import qualified Hunt.ContextIndex           as CIx
import           Hunt.DocTable               (DocTable)
import qualified Hunt.DocTable               as Dt
import           Hunt.Interpreter.Command    (CmdError (..))
import           Hunt.Query.Fuzzy            (FuzzyConfig)
import qualified Hunt.Query.Fuzzy            as F
import           Hunt.Query.Intermediate     (Intermediate)
import qualified Hunt.Query.Intermediate     as I
import           Hunt.Query.Language.Grammar
import           Hunt.Query.Result           (Result)

import qualified System.Log.Logger           as Log

-- ------------------------------------------------------------
-- Logging
-- ------------------------------------------------------------

-- | Name of the module for logging purposes.
modName :: String
modName = "Hunt.Query.Processor"

-- | Log a message at 'DEBUG' priority.
debugM :: String -> IO ()
debugM = Log.debugM modName
{-
-- | Log a message at 'WARNING' priority.
warningM :: String -> IO ()
warningM = Log.warningM modName

-- | Log a message at 'ERROR' priority.
errorM :: String -> IO ()
errorM = Log.errorM modName
-}
-- ------------------------------------------------------------
-- Configuration and state for the query processor
-- ------------------------------------------------------------

-- | Query processor configuration.
data ProcessConfig
  = ProcessConfig
    { fuzzyConfig   :: ! FuzzyConfig -- ^ The configuration for fuzzy queries.
    , optimizeQuery :: ! Bool        -- ^ Optimize the query before processing (default: @False@).
    , wordLimit     :: ! Int         -- ^ The maximum number of words used from a prefix. @0@ = no limit (default: @100@).
    , docLimit      :: ! Int         -- ^ The maximum number of documents taken into account. @0@ = no limit (default: @500@).
    }

-- ------------------------------------------------------------

instance Default ProcessConfig where
  def = ProcessConfig def False 100 500

-- ------------------------------------------------------------

instance Binary ProcessConfig where
  put (ProcessConfig fc o l d)
    = Bin.put fc >> Bin.put o >> Bin.put l >> Bin.put d
  get
    = ProcessConfig <$> Bin.get <*> Bin.get <*> Bin.get <*> Bin.get

-- ------------------------------------------------------------

-- | The internal state of the query processor.
data ProcessEnv
  = ProcessEnv
    { psConfig   :: ! ProcessConfig           -- ^ The configuration for the query processor.
    , psContexts :: ! [Context]               -- ^ The current list of contexts.
    , psIndex    ::   ContextMap Occurrences  -- ^ The index to search.
    , psSchema   ::   Schema                  -- ^ Schema / Schemas for the contexts.
    }

-- ------------------------------------------------------------
-- Processor monad
-- ------------------------------------------------------------

type QueryIndex    = ContextMap     Occurrences

-- | the processor monad
newtype ProcessorT m a = PT { runProcessor :: ReaderT ProcessEnv (ErrorT CmdError m) a }
  deriving (Applicative, Monad, MonadIO, Functor, MonadReader ProcessEnv, MonadError CmdError)

instance MonadTrans ProcessorT where
  lift = PT . lift . lift

type Processor = ProcessorT IO

-- ------------------------------------------------------------
-- Helper
-- ------------------------------------------------------------

processError ::   Int -> Text -> Processor a
processError n msg
    = throwError $ ResError n msg

{-
unless' ::   Bool -> Int -> Text -> Processor ()
unless' b code text = unless b $ processError code text
-}

getContexts ::   Processor [Context]
getContexts = ask >>= return . psContexts

getFuzzyConfig ::   Processor FuzzyConfig
getFuzzyConfig = ask >>= return . fuzzyConfig . psConfig

getIx ::   Processor QueryIndex
getIx = ask >>= return . psIndex

getSchema ::   Processor Schema
getSchema = ask >>= return . psSchema

-- | Get the schema associated with that context/index.
--
--   /Note/: This fails if the schema does not exist.
getContextSchema ::   Context -> Processor ContextSchema
getContextSchema c = do
  schema <- getSchema
  case M.lookup c schema of
    Just cs -> return cs
    _       -> processError 420 ("Context does not exist in schema: " `T.append` c)


-- | Normalizes the search value with respect to the schema context type.
--   First runs the validator that throws an error for invalid values,
--   then runs the normalizers associated with the context.
normQueryCx :: Context -> Text -> Processor Text
normQueryCx c t = do
  s <- getContextSchema c
  -- apply context type validator
  if (validate . ctValidate . cxType $ s) t
    then do
      liftIO . debugM . debugMsg $ s
      -- apply context schema normalizer
      return $ norm s
    else processError 400 $ errorMsg s
    where
  norm s     = normalize' (cxNormalizer s) t
  debugMsg s = T.unpack $ T.concat [ "query normalizer: ", c, ": [", t, "=>", norm s, "]"]
  errorMsg s = T.concat ["value incompatible with context type: ", c, ":", t, "(", T.pack . show $ ctName . cxType $ s,")"]

-- | Normalizes value with respect to multiple contexts.
normQueryCxs ::   [Context] -> Text -> Processor [(Context, Text)]
normQueryCxs cs t  = mapM (\c -> normQueryCx c t >>= \nt -> return (c, nt)) cs

-- | Initialize the state of the processor.
initProcessor :: ProcessConfig -> QueryIndex -> Schema -> ProcessEnv
initProcessor cfg ix s
  = ProcessEnv cfg cxs ix s
  where -- XXX: kind of inefficient
  cxs = filter (\c -> fromMaybe False $ M.lookup c s >>= return . cxDefault) $ CIx.contexts' ix

-- ------------------------------------------------------------
-- Processor code
-- ------------------------------------------------------------

-- XXX: write in terms of processQuery'
-- | Process a query and return the unranked search results.
--
--   Initialize the environment with 'initProcessor'.
--
--   The results can be ranked with the default 'Hunt.Query.Ranking.rank' function.
processQuery :: (DocTable d, Dt.DValue d ~ e)
              => ProcessEnv -> d -> Query -> IO (Either CmdError (Result e))
processQuery st d q = runErrorT . runReaderT (runProcessor processToRes) $ st
    where
    oq = if optimizeQuery (psConfig st) then optimize q else q
    processToRes = process oq >>= \ir -> I.toResult d ir

-- | Generic 'processQuery' with a function to create the final result from an 'Intermediate'.
processQuery' :: (Intermediate -> Processor e) -> ProcessEnv -> Query -> IO (Either CmdError e)
processQuery' f st q = runErrorT . runReaderT (runProcessor processToRes) $ st
    where
    oq = if optimizeQuery (psConfig st) then optimize q else q
    processToRes = process oq >>= f

-- | Works like 'processQuery', but only returns the 'DocId's.
processQueryDocIds :: ProcessEnv -> Query -> IO (Either CmdError DocIdSet)
processQueryDocIds = processQuery' (return . DS.fromList . DM.keys)

-- | Process a Query and construct an intermediate result.
--   This can be further transformed to a final 'Result' with 'Hunt.Query.Intermediate.toResult'.
process :: Query -> Processor Intermediate
process o = case o of
  QWord QCase w       -> forAllContexts . processWordCase      $ w
  QWord QNoCase w     -> forAllContexts . processWordNoCase    $ w
  QWord QFuzzy w      -> processFuzzyWord                      $ w
  QPhrase QCase w     -> forAllContexts . processPhraseCase    $ w
  QPhrase QNoCase w   -> forAllContexts . processPhraseNoCase  $ w
  QPhrase QFuzzy w    -> processPhraseFuzzy  $ w
  QContext c q        -> processContexts c q
  QBinary op q1 q2    -> do -- XXX: maybe parallel
                          pq1 <- process q1
                          pq2 <- process q2
                          processBin op pq1 pq2
  QRange l h          -> processRange l h
  QBoost w q          -> process q >>= processBoost w


-- TODO: previously rdeepseq
-- TODO: parallelize mapM
-- | Evaluate (the query) for all contexts.
forAllContexts :: ([Context] -> Processor Intermediate) -> Processor Intermediate
forAllContexts f = getContexts >>= f

forAllContexts' :: (Context -> Processor Intermediate) -> Processor Intermediate
forAllContexts' f = getContexts >>= mapM f >>= return . I.merges


{-
-- version with explicit state
forAllContexts' :: (QueryIndexCon)
-- | Try to evaluate the query for all contexts in parallel.
--   version with implizit state
               => (Context -> ProcessEnv i -> Processor Intermediate)
               -> Processor Intermediate
forAllContexts' f = getContexts >>= mapM (\c -> get >>= f c) >>= return . I.merges
-}

-- ------------------------------------------------------------
-- Word query
-- ------------------------------------------------------------

-- | Process a single, case-insensitive word by finding all documents
--   which contain the word as prefix.
processWord :: TextSearchOp -> Text -> [Context]
            -> Processor Intermediate
processWord op q cs = do
  st <- ask
  ix <- getIx
  cq <- normQueryCxs cs q
  -- TODO: limitWords on context-basis?
  res <- CIx.searchWithCxsNormalized op cq ix
  return . I.fromListCxs q . map (second (limitWords st)) $ res

-- | Case Sensitive variant of process Word
processWordCase :: Text -> [Context]
                -> Processor Intermediate
processWordCase = processWord PrefixCase

-- | Case Insensitive variant of process Word
processWordNoCase :: Text -> [Context]
                  -> Processor Intermediate
processWordNoCase = processWord PrefixNoCase

-- | Calculates a set of Fuzzy words and queries for all of them
--   NOTE: this might be very inefficient
--   XXX TODO: Optimize Performance
--               - mapConcurrent
--               - upper bound for size of fuzzyset
processFuzzyWord ::   Text -> Processor Intermediate
processFuzzyWord q = do
  cfg <- getFuzzyConfig
  is <- mapM (forAllContexts . processWordNoCase . fst) $ fuzzySet cfg
  return . I.merges $ is
  where
  fuzzySet cfg = (q,0):(F.toList $ F.fuzz cfg q)

-- ------------------------------------------------------------
-- Phrase Query
-- ------------------------------------------------------------

-- | Process a phrase.
processPhrase :: TextSearchOp
              -> Text -> [Context]
              -> Processor Intermediate
processPhrase op q cs = do
    ix  <- getIx
    ims <- mapM (\c -> processPhraseInternal (meaningfulName ix c) q c) cs
    return . I.merges $ ims
    where
    -- TODO: normalization?
    -- XXX: no limit for phrase queries? a phrase query is already really restrictive...
    meaningfulName ix c t = CIx.searchWithCx op c t ix

processPhraseCase   ::   Text -> [Context] -> Processor Intermediate
processPhraseCase   = processPhrase Case

processPhraseNoCase ::   Text -> [Context] -> Processor Intermediate
processPhraseNoCase = processPhrase NoCase

processPhraseFuzzy  ::   Text -> Processor Intermediate
processPhraseFuzzy q = do
  cfg <- getFuzzyConfig
  is <- mapM (forAllContexts . processPhraseNoCase . fst) $ fuzzySet cfg
  return . I.merges $ is
  where
  fuzzySet cfg = (q,0):(F.toList $ F.fuzz cfg q)


-- | Process a phrase query by searching for every word of the phrase and comparing their positions.
processPhraseInternal :: (Text -> Processor RawResult) -> Text -> Context
                      -> Processor Intermediate
processPhraseInternal f q c = do
  result <- resultM
  if DM.null result
    then return I.empty
    else do
      pph <- processPhrase' ws 1 result
      return $ I.fromList q c [(q, pph)]
  where
  resultM = do
    fw <- f w
    return . Occ.merges $ map snd fw
  (w:ws) = T.words q
  processPhrase' :: [Text] -> Position -> Occurrences -> Processor Occurrences
  processPhrase' [] _ o = return o
  processPhrase' (x:xs) p o = do
    fx <- f x
    processPhrase' xs (p+1) (DM.filterWithKey (nextWord $ map snd fx) o)
    where
      nextWord :: [Occurrences] -> DocId -> Positions -> Bool
      nextWord [] _ _  = False
      nextWord no d np = maybe False hasSuccessor $ DM.lookup d (Occ.merges no)
          where
            hasSuccessor :: Positions -> Bool
            hasSuccessor w' = Pos.foldr (\cp r -> r || Pos.member (cp + p) w') False np

-- ------------------------------------------------------------
-- Range Query
-- ------------------------------------------------------------
processRange ::   Text -> Text -> Processor Intermediate
processRange l h = forAllContexts' range
  where
  range c = do
    low     <- normQueryCx c l
    high    <- normQueryCx c h
    -- range validation is not that easy because of different types.
    -- values form a valid range
    --unless' (rangeValidator cType ls' hs'
    --  400 $ "invalid range for context: " `T.append` rangeText

    -- range processing here should always be the same
    -- different behaviours for special cases like location
    -- search should be implemented in an own index proxy
    processRange' low high c

  processRange' ::   Text -> Text -> Context -> Processor Intermediate
  processRange' lo hi c = do
    st <- ask
    ix <- getIx
    -- TODO: limit on context basis
    res <- CIx.lookupRangeCx c lo hi ix
    return . I.fromList lo c . limitWords st $ res -- FIXME: check I.fromList - correct term

-- ------------------------------------------------------------
-- Operators
-- ------------------------------------------------------------

-- | Process a binary operator by caculating the union or the intersection of the two subqueries.
processBin :: BinOp -> Intermediate -> Intermediate -> Processor Intermediate
processBin And    i1 i2 = return $ I.intersection i1 i2
processBin Or     i1 i2 = return $ I.union        i1 i2
processBin AndNot i1 i2 = return $ I.difference   i1 i2


-- | Process query boosting
processBoost :: Score -> Intermediate -> Processor Intermediate
processBoost b i = return $ DM.map (second (b:)) i

-- | Process a context query.
processContexts :: [Context] -> Query -> Processor Intermediate
processContexts cs q = do
  schema <- getSchema
  let invalidContexts = filter (not . flip M.member schema) cs
  if null invalidContexts
    then local setCx $ process q
    else processError 404 $ "mentioned context(s) do not exist: " -- schema to be precise
                          `T.append` (T.pack . show $ invalidContexts)
  where
  setCx cfg = cfg { psContexts = cs}

-- ------------------------------------------------------------
-- Helper
-- ------------------------------------------------------------

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
limitWords              :: ProcessEnv -> RawResult -> RawResult
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

  -- FIXME: this is not what the description says it is!?
  --        'psTotal' is the number of docs and not set atm.
  --        looks like an unnecessary normalisation?
  calcScore             :: (Word, Occurrences) -> (Double, (Word, Occurrences))
  --calcScore w@(_, o)    = (log (fromIntegral (psTotal s) / fromIntegral (DM.size o)), w)
  calcScore w@(_, o)    = (fromIntegral $ DM.size o, w) --(log (fromIntegral (psTotal s) / fromIntegral (DM.size o)), w)

-- ------------------------------------------------------------

-- | Limit the number of docs in a raw result
limitDocs               :: Int -> RawResult -> RawResult
limitDocs _     []      = []
limitDocs limit _
    | limit <= 0        = []
limitDocs limit (x:xs)  = x : limitDocs (limit - DM.size (snd x)) xs

-- ------------------------------------------------------------

-- old range stuff

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

-- ------------------------------------------------------------
