{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}

-- ----------------------------------------------------------------------------
{- |
  The query processor to perform 'Query's.

  There are three variants of the query interperter.

  The simplest one gives an unscored set of DocIds. This is e.g. used
  in the deleteByQuery command.

  The standard interperter for searching documents computes a set of
  DocIds with an associated Score. The result is sorted by the score.

  The third one returns a set of scored words for completion commands.
-}
-- ----------------------------------------------------------------------------

module Hunt.Query.Processor
    ( processQueryScoredDocs
    , processQueryUnScoredDocs
    , processQueryScoredWords
    , initProcessor

    , ProcessConfig (..)
    , ProcessEnv
    )

where

import           Control.Applicative
import           Control.Monad.Error
import           Control.Monad.Reader

import           Data.Binary                 (Binary)
import qualified Data.Binary                 as Bin
import           Data.Default
import qualified Data.List                   as L
import qualified Data.Map                    as M
import           Data.Maybe
import           Data.Monoid                 (Monoid(..), (<>))
import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Hunt.Common.BasicTypes      (Context, Word, TextSearchOp(..))
import           Hunt.ContextIndex           (ContextMap)
import qualified Hunt.ContextIndex           as CIx
import           Hunt.Index.Schema
import           Hunt.Interpreter.Command    (CmdError (..))
import           Hunt.Query.Fuzzy            (FuzzyConfig)
import           Hunt.Query.Intermediate
import           Hunt.Query.Language.Grammar
import           Hunt.Scoring.SearchResult   (ScoredDocs, UnScoredDocs)
import           Hunt.Utility                (showText)

import qualified System.Log.Logger           as Log

-- import           Debug.Trace

-- ------------------------------------------------------------
-- Logging
-- ------------------------------------------------------------

-- | Name of the module for logging purposes.
modName :: String
modName = "Hunt.Query.Processor"

-- | Log a message at 'DEBUG' priority.
debugM :: String -> IO ()
debugM = Log.debugM modName

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
      { psConfig   :: ! ProcessConfig  -- ^ The configuration for the query processor.
      , psContexts :: ! [Context]      -- ^ The current list of contexts.
      , psIndex    ::   ContextMap     -- ^ The index to search.
      }

-- ------------------------------------------------------------
-- Processor monad
-- ------------------------------------------------------------

type QueryIndex
    = ContextMap

-- | the processor monad
newtype ProcessorT m a
    = PT { runProcessor :: ReaderT ProcessEnv (ErrorT CmdError m) a
         }
      deriving (Applicative, Monad, MonadIO, Functor, MonadReader ProcessEnv, MonadError CmdError)

instance MonadTrans ProcessorT where
    lift = PT . lift . lift

type Processor
    = ProcessorT IO

-- ------------------------------------------------------------
-- Helper
-- ------------------------------------------------------------

queryError ::   Int -> Text -> Processor a
queryError n msg
    = throwError $ ResError n msg

getContexts ::   Processor [Context]
getContexts
    = asks psContexts

getIx :: Processor QueryIndex
getIx
    = asks psIndex

getSchema :: Processor Schema
getSchema
    = getIx >>= return . (M.map fst) . CIx.cxMap

-- | Get the schema associated with that context/index.
--
--   /Note/: This fails if the schema does not exist.

getContextSchema ::   Context -> Processor ContextSchema
getContextSchema c
    = do schema <- getSchema
         case M.lookup c schema of
           Just cs -> return cs
           _       -> queryError 420 ("Context does not exist in schema: " <> c)


-- | Normalizes the search value with respect to the schema context type.
--   First runs the validator that throws an error for invalid values,
--   then runs the normalizers associated with the context.

normQueryCx :: Context -> Text -> Processor (Maybe Text)
normQueryCx c t
    = do s <- getContextSchema c
         -- apply context type validator
         if (validate . ctValidate . cxType $ s) t
           then
               do liftIO . debugM . debugMsg $ s
                  -- apply context schema normalizer
                  return . Just . norm $ s
           else
               return Nothing
    where
      norm s
          = normalize' (cxNormalizer s) t

      debugMsg s
          = T.unpack $ T.concat [ "query normalizer: ", c, ": [", t, "=>", norm s, "]"]

-- | Initialize the state of the processor.
initProcessor :: ProcessConfig -> QueryIndex -> ProcessEnv
initProcessor cfg ix
    = ProcessEnv cfg cxs ix
    where
      s = CIx.mapToSchema ix
      cxs = filter (\c -> fromMaybe False $ M.lookup c s >>= return . cxDefault)
            $ CIx.contexts ix

-- ------------------------------------------------------------

processQueryUnScoredDocs :: ProcessEnv -> Query -> IO (Either CmdError UnScoredDocs)
processQueryUnScoredDocs = processQueryScoredResult evalUnScoredDocs

-- | evaluate a query into a UnScoredDocs result
--
-- all info about contexts, words and positions and the score of docs
-- are removed by the aggregation.
--
-- This evaluator is called by commands which need to compute just a set of documents,
-- e.g. DeleteByQuery. When calling evalUnScoredDocs the 'ProcessEnv' value
-- should be configured such that the limit for document to taken into account
-- is set to infinity (represented as @0@ in the config), else the result set
-- may not be complete.
--
-- In that case it becomes easy to build a query witch acts as a denial of service
-- attack, because the intermediate results become too large to be processed.
-- So these queries must be used in applications rather carefully, e.g. a user should
-- not be able to construct these types of queries by filling in some input fields in a web interface.

evalUnScoredDocs :: Query -> Processor UnScoredDocs
evalUnScoredDocs q
    | isPrimaryQuery q
        = do res <- forallCx (evalPrimary q)
             aggregateToScoredResult res

evalUnScoredDocs (QRange lb ub)
    = do res <- forallCx (evalRange lb ub)
         aggregateToScoredResult res

evalUnScoredDocs (QSeq op qs)
    | isLocalCxOp op
        = do res <- forallCxLocal
                    ( evalSeq' op                     -- do the position aware operation
                      <$> mapM evalScoredRawDocs qs ) -- switch to the raw evaluator due to positions
             aggregateToScoredResult res              -- and aggregate res to UnScoredDocs
    | otherwise
        = evalSeq op                                  -- do the result combination
          <$> mapM evalUnScoredDocs qs                  -- for the args stay in evaluator

evalUnScoredDocs (QContext cxs q)
    = withCxs cxs $ evalUnScoredDocs q

evalUnScoredDocs (QBoost _w q)
    = evalUnScoredDocs q

evalUnScoredDocs q@QPhrase{}              -- QPhrase is transformed into QFullWord or QSeq Phrase
    = normQuery q >>= evalUnScoredDocs

evalUnScoredDocs q@QBinary{}              -- QBin is transformed into QSeq
    = normQuery q >>= evalUnScoredDocs

evalUnScoredDocs q
    = queryEvalError q

-- ------------------------------------------------------------

processQueryScoredDocs :: ProcessEnv -> Query -> IO (Either CmdError ScoredDocs)
processQueryScoredDocs = processQueryScoredResult evalScoredDocs'

-- | evaluate a query into a ScoredDocs result
--
-- all info about contexts, words and positions is removed by the aggregation,
-- just a set of DocIds and associated scores is computed

evalScoredDocs :: Query -> Processor ScoredDocs
evalScoredDocs q
    | isPrimaryQuery q
        = do res <- forallCx (evalPrimary q)
             aggregateToScoredResult res

evalScoredDocs (QRange lb ub)
    = do res <- forallCx (evalRange lb ub)
         aggregateToScoredResult res

evalScoredDocs (QSeq op qs)
    | isLocalCxOp op
        = do res <- forallCxLocal
                    ( evalSeq' op                     -- do the position aware operation
                      <$> mapM evalScoredRawDocs' qs ) -- switch to the raw evaluator due to positions
             aggregateToScoredResult res                -- and aggregate res to ScoredDocs
    | otherwise
        = evalSeq op                                  -- do the result combination
          <$> mapM evalScoredDocs' qs                  -- for the args stay in evaluator

evalScoredDocs (QContext cxs q)
    = withCxs cxs $ evalScoredDocs' q

evalScoredDocs (QBoost w q)
    = boost w <$> evalScoredDocs' q

evalScoredDocs q@QPhrase{}              -- QPhrase is transformed into QFullWord or QSeq Phrase
    = normQuery q >>= evalScoredDocs

evalScoredDocs q@QBinary{}              -- QBin is transformed into QSeq
    = normQuery q >>= evalScoredDocs

evalScoredDocs q
    = queryEvalError q

-- --------------------
-- {- switch off trace

evalScoredDocs' :: Query -> Processor ScoredDocs
evalScoredDocs' = evalScoredDocs
{-# INLINE evalScoredDocs' #-}

-- -}
{- switch on trace the evaluation

evalScoredDocs' :: Query -> Processor ScoredDocs
evalScoredDocs' q = trc <$> evalScoredDocs q
    where
      trc res = traceShow ("evalScoredDocs: "::String,q  ) $
                traceShow ("evalScoredDocs: "::String,res) $ res
-- -}
-- ------------------------------------------------------------

processQueryScoredWords :: ProcessEnv -> Query -> IO (Either CmdError ScoredWords)
processQueryScoredWords = processQueryScoredResult evalScoredWords'

-- | evaluate a query into a ScoredWords result
--
-- all info about contexts, words and positions is removed by the aggregation,
-- just a set of words and associated scores is computed.
--
-- the words found are suggestions for the last primitive prefix in the query

evalScoredWords :: Query -> Processor ScoredWords
evalScoredWords q
    | isPrimaryQuery q
        = do res <- forallCx (evalPrimary q)
             aggregateToScoredResult res

evalScoredWords (QRange lb ub)
    = do res <- forallCx (evalRange lb ub)
         aggregateToScoredResult res

evalScoredWords (QSeq Or qs)                           -- for completions just search
    = evalScoredWords' (last qs)                       -- for rightmost subquery

evalScoredWords (QSeq AndNot qs)                       -- for completions just search
    = evalScoredWords' (last qs)                       -- for rightmost subquery

evalScoredWords (QSeq And qs)                          -- for completions
    = do docs <- evalUnScoredDocs  (mkQ $ init qs)     -- eval the set of docs for all but the last q
         res  <- evalScoredRawDocs (      last qs)     -- eval the last q as ScoredRawDos
         aggregateToScoredResult                       -- aggregate to ScoredWords
           $ fmap (filterByDocSet docs) res            -- restrict result to docs found
      where
        mkQ [q'] = q'
        mkQ qs'  = QSeq And qs'

evalScoredWords (QSeq op qs)
    | isLocalCxOp op
        = do res <- forallCxLocal
                    ( evalSeq' op                      -- do the position aware operation
                      <$> mapM evalScoredRawDocs' qs ) -- switch to the raw evaluator due to positions
             aggregateToScoredResult res               -- and aggregate res to ScoredWords

evalScoredWords (QContext cxs q)
    = withCxs cxs $ evalScoredWords' q

evalScoredWords (QBoost w q)
    = boost w <$> evalScoredWords' q

evalScoredWords q@QPhrase{}              -- QPhrase is transformed into QFullWord or QSeq Phrase
    = normPhraseQuery q >>= evalScoredWords

evalScoredWords q@QBinary{}              -- QBin is transformed into QSeq
    = normQuery q >>= evalScoredWords

evalScoredWords q
    = queryEvalError q

-- --------------------
-- {- switch off trace

evalScoredWords' :: Query -> Processor ScoredWords
evalScoredWords' = evalScoredWords
{-# INLINE evalScoredWords' #-}

-- -}
{- switch on trace of evaluation

evalScoredWords' :: Query -> Processor ScoredWords
evalScoredWords' q = trc <$> evalScoredWords q
    where
      trc res = traceShow ("evalScoredWords: "::String,q  ) $
                traceShow ("evalScoredWords: "::String,res) $ res
-- -}
-- ------------------------------------------------------------

-- | evaluate a query into a context aware raw docs result
--
-- This evaluator is called from 'evalScoredDocs', 'evalUnScoredDocs' and 'evalScoredWords'
-- in the case of Phrase-, Follow- and Near-queries.
--
-- No info about contexts, words and positions is removed by the aggregation.
-- 'evalScoredRawDocs' always runs in a single context at a time,
-- because the position information does not have any meaning across contexts
--
-- Therefore QContext subqueries become meaningless within Phrase-, Follow- and Near-queries

evalScoredRawDocs :: Query -> Processor (ScoredCx ScoredRawDocs)
evalScoredRawDocs q
    | isPrimaryQuery q
        = forallCx (evalPrimary q)

evalScoredRawDocs (QRange lb ub)
    = forallCx (evalRange lb ub)

evalScoredRawDocs (QSeq op qs)
    = evalSeq' op
      <$> mapM evalScoredRawDocs' qs

evalScoredRawDocs (QContext cxs q)
    = restrictCxs cxs $ evalScoredRawDocs' q

evalScoredRawDocs (QBoost w q)
    = boost w <$> evalScoredRawDocs' q

evalScoredRawDocs q@QPhrase{}              -- QPhrase is transformed into QFullWord or QSeq Phrase
    = normQuery q >>= evalScoredRawDocs

evalScoredRawDocs q@QBinary{}              -- QBin is transformed into QSeq
    = normQuery q >>= evalScoredRawDocs

evalScoredRawDocs q
    = queryEvalError q

-- --------------------
-- {- switch off trace

evalScoredRawDocs' :: Query -> Processor (ScoredCx ScoredRawDocs)
evalScoredRawDocs' = evalScoredRawDocs
{-# INLINE evalScoredRawDocs' #-}

-- -}
{- switch on trace of evaluation

evalScoredRawDocs' :: Query -> Processor (ScoredCx ScoredRawDocs)
evalScoredRawDocs' q = trc <$> evalScoredRawDocs q
    where
      trc res = traceShow ("evalScoredRawDocs: "::String,q  ) $
                traceShow ("evalScoredRawDocs: "::String,res) $ res
-- -}
-- ------------------------------------------------------------

-- run a query evaluation

processQueryScoredResult :: (q -> Processor r) -> ProcessEnv -> q -> IO (Either CmdError r)
processQueryScoredResult eval st q
    = runErrorT . runReaderT (runProcessor $ eval q) $ st


-- ------------------------------------------------------------
--
-- query normalization: transform "old" queries into generalized new form

normQuery :: Query -> Processor Query
normQuery (QPhrase op w)
    = return . QSeq Phrase . L.map (QFullWord op) $ T.words w

normQuery q@(QBinary op _q1 _q2)
    = return . QSeq op . collect op q $ []
      where
        collect
            | isAssocOp     op = collectAssoc
            | isLeftAssocOp op = collectLeftAssoc
            | otherwise        = \ _op q' -> (q':)

normQuery q
    = return q


normPhraseQuery :: Query -> Processor Query
normPhraseQuery (QPhrase op w)
    = return . QSeq Phrase $ normPhrase (T.words w) []
    where
      normPhrase (x:[]) r = r ++ [QWord op x]
      normPhrase (x:xs) r = normPhrase xs $
                            r ++ [QFullWord op x]
      normPhrase []     r = r
normPhraseQuery q
    = return q



collectAssoc :: BinOp -> Query -> [Query] -> [Query]
collectAssoc op (QBinary op' q1 q2)
    | op == op'
        = collectAssoc op q1 . collectAssoc op q2
collectAssoc _ q
    = (q :)

collectLeftAssoc :: BinOp -> Query -> [Query] -> [Query]
collectLeftAssoc op (QBinary op' q1 q2)
    | op == op'
        = collectAssoc op q1 . (q2 :)
collectLeftAssoc _ q
    = (q :)

isAssocOp :: BinOp -> Bool
isAssocOp AndNot = False
isAssocOp _      = True

isLeftAssocOp :: BinOp -> Bool          -- currently AndNot is left assoc
isLeftAssocOp = not . isAssocOp         -- all others are assoc ops

isLocalCxOp :: BinOp -> Bool
isLocalCxOp Phrase   = True
isLocalCxOp Follow{} = True
isLocalCxOp Near{}   = True
isLocalCxOp _        = False

-- ------------------------------------------------------------

-- eval query combinators

evalSeq :: (ScoredResult r) => BinOp -> [r] -> r
evalSeq Or     = evalOr
evalSeq And    = evalAnd
evalSeq AndNot = evalAndNot
evalSeq _op    = const mempty

evalSeq' :: BinOp -> [ScoredCx ScoredRawDocs] -> ScoredCx ScoredRawDocs
evalSeq' Phrase     = evalSequence id
evalSeq' (Follow d) = evalFollow   id d
evalSeq' (Near   d) = evalNear     id d
evalSeq' op         = evalSeq op

-- | restrict the current context for a query
--
-- used in evaluating phrase-, follow- and near- queries
-- there it's meaningless to use other than the current contexts

restrictCxs :: [Context] -> Processor r -> Processor r
restrictCxs cxs p
    = do cxs0 <- getContexts
         withCxs cxs $                                           -- check for legal contexts
                 local (setCx $ cxs0 `L.intersect` L.nub cxs) p  -- restrict contexts to cxs
    where
      setCx cxs' cfg
          = cfg {psContexts = cxs'}

-- | set the context for a part of a query evaluation

withCxs :: [Context] -> Processor r -> Processor r
withCxs cxs p
    = do icxs <- invalidContexts <$> getSchema
         if L.null icxs
            then local setCx $ p
            else queryError 404
                 $ "mentioned context(s) do not exist: "
                   <> showText icxs
    where
      invalidContexts sc
          = filter (\ c -> not (c `M.member` sc)) cxs
      setCx cfg
          = cfg {psContexts = L.nub cxs}

-- | execute a command (query) for all contexts and give the current context
-- as extra argument

forallCx :: (ScoredResult r) => (Context -> Processor r) -> Processor r
forallCx action
    = do cxs <- getContexts
         res <- mapM action cxs
         return $ mconcat res

-- | execute a command for all contexts, but restrict the set of contexts
-- to the current context before executing the command
--
-- So the loop over all contexts (forallCx) called in the evaluation of primary
-- queries is switched off

forallCxLocal :: (ScoredResult r) => Processor r -> Processor r
forallCxLocal action
    = do cxs <- getContexts
         res <- mapM f' cxs
         return $ mconcat res
    where
      f' cx
          = withCxs [cx] action

isPrimaryQuery :: Query -> Bool
isPrimaryQuery QWord{}     = True
isPrimaryQuery QFullWord{} = True
isPrimaryQuery _           = False

queryEvalError :: Query -> Processor r
queryEvalError q
    = queryError 501 $ "Hunt.Query.Processor: query can't be evaluated " <> showText q

-- ------------------------------------------------------------

aggregateToScoredResult :: (Aggregate a (ScoredCx b), ScoredResult b) =>
                           a -> ProcessorT IO b
aggregateToScoredResult res
    = do cxScores <- contextWeights <$> getSchema
         return $ boostAndAggregateCx cxScores (aggregate res)


-- eval basic queries

evalPrimary :: Query -> Context -> Processor (ScoredCx ScoredRawDocs)
evalPrimary (QWord QCase w) cx
    = searchCx PrefixCase w cx

evalPrimary (QWord QNoCase w) cx
    = searchCx PrefixNoCase w cx

evalPrimary (QWord QFuzzy w) cx         -- TODO: QFuzzy is processed like nocase search
    = searchCx PrefixNoCase w cx

evalPrimary (QFullWord QCase w) cx
    = searchCx Case w cx

evalPrimary (QFullWord QNoCase w) cx
    = searchCx NoCase w cx

evalPrimary (QFullWord QFuzzy w) cx     -- TODO: QFuzzy is processed like nocase search
    = searchCx NoCase w cx

evalPrimary q _cx
    = queryError 501 $ "evalPrimary: not a primary query: " <> showText q

searchCx :: TextSearchOp -> Word -> Context -> Processor (ScoredCx ScoredRawDocs)
searchCx op w' cx
    = do mw <- normQueryCx cx w'                         -- normalize the word with respect to context
         case mw of
           Nothing  -> return $ fromCxRawResults []      -- if normalization not possible, return empty result
           (Just w) -> searchCx' w
    where
      searchCx' w
        = do
          limit <- asks (docLimit . psConfig)            -- get the max. # of docs
          ix    <- getIx                                 -- get the context search index
          rawr  <- limitRawResult limit
                   <$> CIx.searchWithCxSc op cx w ix     -- do the real search and limit result
          return $ fromCxRawResults [(cx, rawr)]
                                                         -- convert the result to a ScoredResult
                                                         -- the score comes from a similarity test

evalRange :: Word -> Word -> Context -> Processor (ScoredCx ScoredRawDocs)
evalRange lb0 ub0 cx
    = do mlb    <- normQueryCx cx lb0                         -- normalize the word with respect to context
         mub    <- normQueryCx cx ub0                         -- normalize the word with respect to context
         case (mlb, mub) of
           (Just lb, Just ub) -> evalRange' lb ub
           _                  -> return $ fromCxRawResults [] -- if one of the words fails validation, return empty result
    where
      evalRange' lb ub
        = do
          limit <- asks (docLimit . psConfig)
          ix    <- getIx
          rawr  <- limitRawResult limit
                   <$> CIx.lookupRangeCxSc cx lb ub ix
          return $ fromCxRawResults [(cx, rawr)]

-- ------------------------------------------------------------

{-
-- | Log a message at 'WARNING' priority.
warningM :: String -> IO ()
warningM = Log.warningM modName

-- | Log a message at 'ERROR' priority.
errorM :: String -> IO ()
errorM = Log.errorM modName
-}

-- ------------------------------------------------------------
