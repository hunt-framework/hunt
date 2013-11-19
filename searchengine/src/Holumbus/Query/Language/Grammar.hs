-- ----------------------------------------------------------------------------

{- |
  Module     : Holumbus.Query.Language.Grammar
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

  The Holumbus query language definition.

  The specific syntax of any query language can be designed independently
  by creating appropriate parsers. Also see "Holumbus.Query.Language.Parser".

-}

-- ----------------------------------------------------------------------------

module Holumbus.Query.Language.Grammar
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
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Binary           ()

import           Holumbus.Common.BasicTypes as BTy
import           Holumbus.Common.Schema

-- ----------------------------------------------------------------------------

data Query = QWord      TextSearchType Text        -- ^ Word search.
           | QPhrase    TextSearchType Text        -- ^ Phrase search
           | QContext   [(Context)] Query          -- ^ Restrict query to a list of contexts.
           | QNegation  Query                      -- ^ Negate the query.
           | QBinary    BinOp Query Query          -- ^ Combine two queries through a binary operation.
           | QBoost     CWeight Query              -- ^ Weight for Query
           | QRange     Text Text                  -- ^ Range Query
           deriving (Eq, Show)

data TextSearchType = QCase | QNoCase | QFuzzy
  deriving (Eq, Show)

-- | A binary operation.
data BinOp = And  -- ^ Intersect two queries.
           | Or   -- ^ Union two queries.
           | But  -- ^ Filter a query by another, @q1 BUT q2@ is equivalent to @q1 AND NOT q2@.
                  --   This operator is useful for query processing optimizations.
           deriving (Eq, Show)

-- ----------------------------------------------------------------------------
instance ToJSON Query where
  toJSON o = case o of
    QWord op w        -> object . ty "word"    $ [ "op" .= op, "word"   .= w ]
    QPhrase op s      -> object . ty "phrase"  $ [ "op" .= op, "phrase" .= s ]
    QContext c q      -> object . ty "context" $ [ "contexts" .= c , "query" .= q ]
    QNegation q       -> object . ty "not"     $ [ "query"  .= q ]
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
      "but" -> bin But
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
    And -> "and"
    Or  -> "or"
    But -> "but"

instance FromJSON BinOp where
  parseJSON (String s)
    = case s of
      "and" -> return And
      "or"  -> return Or
      "but" -> return But
      _         -> mzero
  parseJSON _ = mzero

-- ----------------------------------------------------------------------------

instance Binary Query where
  put (QWord op s)       = put (0 :: Word8) >> put op >> put s
  put (QPhrase op s)     = put (1 :: Word8) >> put op >> put s
  put (QContext c q)     = put (2 :: Word8) >> put c >> put q
  put (QNegation q)      = put (3 :: Word8) >> put q
  put (QBinary o q1 q2)  = put (4 :: Word8) >> put o >> put q1 >> put q2
  put (QBoost w q)       = put (5 :: Word8) >> put w >> put q
  put (QRange l u)       = put (6 :: Word8) >> put l >> put u

  get = do tag <- getWord8
           case tag of
             0 -> liftM2 QWord      get get
             1 -> liftM2 QPhrase    get get
             2 -> liftM2 QContext   get get
             3 -> liftM  QNegation  get
             4 -> liftM3 QBinary    get get get
             5 -> liftM2 QBoost     get get
             6 -> liftM2 QRange     get get
             _ -> fail "Error while decoding Query"


instance Binary TextSearchType where
  put QCase   = put (0 :: Word8)
  put QNoCase = put (1 :: Word8)
  put QFuzzy  = put (2 :: Word8)

  get = do tag <- getWord8
           case tag of
             0 -> return QCase
             1 -> return QNoCase
             2 -> return QFuzzy
             _ -> fail "Error while decoding BinOp"

instance Binary BinOp where
  put And = put (0 :: Word8)
  put Or  = put (1 :: Word8)
  put But = put (2 :: Word8)

  get = do tag <- getWord8
           case tag of
             0 -> return And
             1 -> return Or
             2 -> return But
             _ -> fail "Error while decoding BinOp"

-- ----------------------------------------------------------------------------

-- | Transforms all @(QBinary And q1 q2)@ where one of @q1@ or @q2@ is a @Negation@ into
-- @QBinary Filter q1 q2@ or @QBinary Filter q2 q1@ respectively.
optimize :: Query -> Query

optimize q@(QBinary And (QWord QNoCase q1) (QWord QNoCase q2))
  | T.toLower q1 `T.isPrefixOf` T.toLower q2 = QWord QNoCase q2
  | T.toLower q2 `T.isPrefixOf` T.toLower q1 = QWord QNoCase q1
  | otherwise = q

optimize q@(QBinary And (QWord QCase q1) (QWord QCase q2))
  | q1 `T.isPrefixOf` q2 = QWord QCase q2
  | q2 `T.isPrefixOf` q1 = QWord QCase q1
  | otherwise = q

optimize q@(QBinary Or (QWord QNoCase q1) (QWord QNoCase q2))
  | T.toLower q1 `T.isPrefixOf` T.toLower q2 = QWord QNoCase q1
  | T.toLower q2 `T.isPrefixOf` T.toLower q1 = QWord QNoCase q2
  | otherwise = q

optimize q@(QBinary Or (QWord QCase q1) (QWord QCase q2))
  | q1 `T.isPrefixOf` q2 = QWord QCase q1
  | q2 `T.isPrefixOf` q1 = QWord QCase q2
  | otherwise = q

optimize (QBinary And q1 (QNegation q2))  = QBinary But (optimize q1) (optimize q2)
optimize (QBinary And (QNegation q1) q2)  = QBinary But (optimize q2) (optimize q1)

optimize (QBinary And q1 q2)             = QBinary And (optimize q1) (optimize q2)
optimize (QBinary Or q1 q2)              = QBinary Or (optimize q1) (optimize q2)
optimize (QBinary But q1 q2)             = QBinary But (optimize q1) (optimize q2)
optimize (QNegation q)                   = QNegation (optimize q)
optimize (QContext cs q)                 = QContext cs (optimize q)

optimize q                               = q

-- | Check if the query arguments comply with some custom predicate.
checkWith                         :: (Text -> Bool) -> Query -> Bool
checkWith f (QWord QNoCase s)     = f s
checkWith f (QPhrase QNoCase s)   = f s
checkWith f (QWord QCase s)       = f s
checkWith f (QPhrase QCase s)     = f s
checkWith f (QWord QFuzzy s)      = f s
checkWith f (QNegation q)         = checkWith f q
checkWith f (QBinary _ q1 q2)     = checkWith f q1 && checkWith f q2
checkWith f (QContext _ q)        = checkWith f q

-- | Returns a list of all terms in the query.
extractTerms                      :: Query -> [Text]
extractTerms (QWord QNoCase s)     = [s]
extractTerms (QWord QCase s)       = [s]
extractTerms (QWord QFuzzy s)      = [s]
extractTerms (QContext _ q)        = extractTerms q
extractTerms (QNegation q)         = extractTerms q
extractTerms (QBinary _ q1 q2)     = extractTerms q1 ++ extractTerms q2
extractTerms _                     = []
