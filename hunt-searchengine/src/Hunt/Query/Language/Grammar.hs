{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------
{- |
  The query language.

  'Query' specifies the complete grammar.

  "Hunt.Query.Language.Parser" provides a parser for plain text queries.
-}
-- ----------------------------------------------------------------------------

module Hunt.Query.Language.Grammar
  (
  -- * Query data types
    Query (..)
  , BinOp (..)
  , TextSearchType (..)

  -- * Optimizing
  , optimize
  , checkWith
  , extractTerms
  )
where

import           Control.Monad

import           Data.Aeson
import           Data.Binary
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Text.Binary       ()

import           Hunt.Common.BasicTypes as BTy

-- ------------------------------------------------------------

-- | The query language.
data Query
  = QWord    TextSearchType Text  -- ^ Word search.
  | QPhrase  TextSearchType Text  -- ^ Phrase search.
  | QContext [Context] Query      -- ^ Restrict a query to a list of contexts.
  | QBinary  BinOp Query Query    -- ^ Combine two queries with a binary operation.
  | QBoost   Weight Query         -- ^ Weight for query.
  | QRange   Text Text            -- ^ Range query.
  deriving (Eq, Show)

-- | The search opeation.
data TextSearchType
  = QCase   -- ^ Case-sensitive search.
  | QNoCase -- ^ Case-insensitive search.
  | QFuzzy  -- ^ Fuzzy search. See "Hunt.Query.Fuzzy" for details.
            --   The query processor allows additional configuration with
            --   'Hunt.Query.Processor.ProcessConfig'.
  deriving (Eq, Show)

-- | A binary operation.
data BinOp
  = And    -- ^ Intersect two queries.
  | Or     -- ^ Union two queries.
  | AndNot -- ^ Filter a query by another.
  deriving (Eq, Show)

-- ------------------------------------------------------------
-- JSON instances
-- ------------------------------------------------------------

instance ToJSON Query where
  toJSON o = case o of
    QWord op w        -> object . ty "word"    $ [ "op" .= op, "word"   .= w ]
    QPhrase op s      -> object . ty "phrase"  $ [ "op" .= op, "phrase" .= s ]
    QContext c q      -> object . ty "context" $ [ "contexts" .= c , "query" .= q ]
    QBinary op q1 q2  -> object . ty' op       $ [ "query1" .= q1, "query2" .= q2 ]
    QBoost  w q       -> object . ty "boost"   $ [ "weight" .= w, "query" .= q ]
    QRange l u        -> object . ty "range"   $ [ "lower" .= l, "upper" .= u ]
    where
    ty' t = (:) ("type" .= t)
    ty  t = ty' (t :: Text)

instance FromJSON Query where
  parseJSON (Object o) = do
    t <- o .: "type"
    case (t :: Text) of
      "word"     -> do
         op <- o .: "op"
         w  <- o .: "word"
         return $ QWord op w
      "phrase"   -> do
         op <- o .: "op"
         w  <- o .: "phrase"
         return $ QPhrase op w
      "context"  -> do
        c <- o .: "contexts"
        q <- o .: "query"
        return $ QContext c q
      "boost"   -> do
        w <- o .: "weight"
        q <- o .: "query"
        return $ QBoost w q
      "range"    -> do
        l <- o .: "lower"
        u <- o .: "upper"
        return $ QRange l u
      "and" -> bin And
      "or"  -> bin Or
      "and not" -> bin AndNot
      _         -> mzero
    where
    bin op = do
      q1 <- o .: "query1"
      q2 <- o .: "query2"
      return $ QBinary op q1 q2
  parseJSON _ = mzero


instance ToJSON TextSearchType where
  toJSON o = case o of
    QCase   -> "case"
    QNoCase -> "nocase"
    QFuzzy  -> "fuzzy"

instance FromJSON TextSearchType where
  parseJSON (String s)
    = case s of
      "case"   -> return QCase
      "nocase" -> return QNoCase
      "fuzzy"  -> return QFuzzy
      _         -> mzero
  parseJSON _ = mzero

instance ToJSON BinOp where
  toJSON o = case o of
    And    -> "and"
    Or     -> "or"
    AndNot -> "and not"

instance FromJSON BinOp where
  parseJSON (String s)
    = case s of
      "and"     -> return And
      "or"      -> return Or
      "and not" -> return AndNot
      _         -> mzero
  parseJSON _ = mzero

-- ------------------------------------------------------------
-- Binary instances
-- ------------------------------------------------------------

instance Binary Query where
  put (QWord op s)       = put (0 :: Word8) >> put op >> put s
  put (QPhrase op s)     = put (1 :: Word8) >> put op >> put s
  put (QContext c q)     = put (2 :: Word8) >> put c >> put q
  put (QBinary o q1 q2)  = put (4 :: Word8) >> put o >> put q1 >> put q2
  put (QBoost w q)       = put (5 :: Word8) >> put w >> put q
  put (QRange l u)       = put (6 :: Word8) >> put l >> put u

  get = do
    tag <- getWord8
    case tag of
      0 -> liftM2 QWord      get get
      1 -> liftM2 QPhrase    get get
      2 -> liftM2 QContext   get get
      4 -> liftM3 QBinary    get get get
      5 -> liftM2 QBoost     get get
      6 -> liftM2 QRange     get get
      _ -> fail "Error while decoding Query"


instance Binary TextSearchType where
  put QCase   = put (0 :: Word8)
  put QNoCase = put (1 :: Word8)
  put QFuzzy  = put (2 :: Word8)

  get = do
    tag <- getWord8
    case tag of
      0 -> return QCase
      1 -> return QNoCase
      2 -> return QFuzzy
      _ -> fail "Error while decoding BinOp"

instance Binary BinOp where
  put And    = put (0 :: Word8)
  put Or     = put (1 :: Word8)
  put AndNot = put (2 :: Word8)

  get = do
    tag <- getWord8
    case tag of
      0 -> return And
      1 -> return Or
      2 -> return AndNot
      _ -> fail "Error while decoding BinOp"

-- ------------------------------------------------------------

-- | Minor query optimizations.
--
--   /Note/: This can affect the ranking.
optimize :: Query -> Query
-- Same prefix in AND query (case-insensitive)
optimize q@(QBinary And (QWord QNoCase q1) (QWord QNoCase q2))
  | T.toLower q1 `T.isPrefixOf` T.toLower q2 = QWord QNoCase q2
  | T.toLower q2 `T.isPrefixOf` T.toLower q1 = QWord QNoCase q1
  | otherwise = q
-- Same prefix in AND query (case-sensitive)
optimize q@(QBinary And (QWord QCase q1) (QWord QCase q2))
  | q1 `T.isPrefixOf` q2 = QWord QCase q2
  | q2 `T.isPrefixOf` q1 = QWord QCase q1
  | otherwise = q
-- Same prefix in OR query (case-insensitive)
optimize q@(QBinary Or (QWord QNoCase q1) (QWord QNoCase q2))
  | T.toLower q1 `T.isPrefixOf` T.toLower q2 = QWord QNoCase q1
  | T.toLower q2 `T.isPrefixOf` T.toLower q1 = QWord QNoCase q2
  | otherwise = q
-- Same prefix in OR query (case-sensitive)
optimize q@(QBinary Or (QWord QCase q1) (QWord QCase q2))
  | q1 `T.isPrefixOf` q2 = QWord QCase q1
  | q2 `T.isPrefixOf` q1 = QWord QCase q2
  | otherwise = q
-- recursive application
optimize (QBinary And    q1 q2) = QBinary And    (optimize q1) (optimize q2)
optimize (QBinary Or     q1 q2) = QBinary Or     (optimize q1) (optimize q2)
optimize (QBinary AndNot q1 q2) = QBinary AndNot (optimize q1) (optimize q2)
optimize (QContext cs q)        = QContext cs (optimize q)
optimize (QBoost w q)           = QBoost w (optimize q)

optimize q                      = q

-- | Check if the query arguments comply with some custom predicate.
checkWith                         :: (Text -> Bool) -> Query -> Bool
checkWith f (QWord _ s)           = f s
checkWith f (QPhrase _ s)         = f s
checkWith f (QBinary _ q1 q2)     = checkWith f q1 && checkWith f q2
checkWith f (QContext _ q)        = checkWith f q
checkWith f (QBoost _ q)          = checkWith f q
checkWith f (QRange s1 s2)        = f s1 && f s2

-- | Returns a list of all terms in the query.
extractTerms                      :: Query -> [Text]
extractTerms (QWord QNoCase s)     = [s]
extractTerms (QWord QCase s)       = [s]
extractTerms (QWord QFuzzy s)      = [s]
extractTerms (QContext _ q)        = extractTerms q
extractTerms (QBinary _ q1 q2)     = extractTerms q1 ++ extractTerms q2
extractTerms _                     = []

-- ------------------------------------------------------------
