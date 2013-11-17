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

  -- * Optimizing
  , optimize
  --, checkWith
  --, extractTerms
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

data Query = QText      TextSearchOp Text     -- ^ Word search.
           -- TODO: phrase with a single constructor, or is there a better abstraction?
           | Phrase     Text                  -- ^ Single case-insensitive phrase.
           | CasePhrase Text                  -- ^ Single case-sensitive phrase.
           | Specifier  [Context] Query       -- ^ Restrict query to a list of contexts.
           | Negation   Query                 -- ^ Negate the query.
           | QBinary    BinOp [Query]         -- ^ Combine two queries through a binary operation.
           deriving (Eq, Show)

-- | new The query language.
{--
data Query = QText    TextSearchOp Word           -- Single Text Queries
           | QContext [(Context, CWeight)] Query  -- Weighted Context Queries
           | QRange   Word Word                   -- Range Queries
           | QBinary  BinOp [Query]               -- Combinator
           | QNegation Query                      -- XXX do we still support this??
           deriving (Eq, Show)
--}

-- | A binary operation.
data BinOp = And  -- ^ Intersect two queries.
           | Or   -- ^ Union two queries.
           | But  -- ^ Filter a query by another, @q1 BUT q2@ is equivalent to @q1 AND NOT q2@.
                  --   This operator is useful for query processing optimizations.
           deriving (Eq, Show)

-- ----------------------------------------------------------------------------
instance ToJSON Query where
  toJSON o = case o of
    QText op w        -> object . ty "wd" $ [ "op"  .= op, "str" .= w ]
    Phrase s          -> object . ty "pi" $ [ "str" .= s ]
    CasePhrase s      -> object . ty "pc" $ [ "str" .= s ]
    Specifier c q     -> object . ty "cx" $ [ "str" .= c , "qry" .= q]
    Negation q        -> object . ty "nt" $ [ "str"  .= q ]
    QBinary op qs     -> object . ty' op  $ [ "qrys" .= qs ]
    where
    ty' t = (:) ("type" .= t)
    ty  t = ty' (t :: Text)


{-- new json
instance ToJSON Query where
  toJSON o = case o of
    QText op w         -> object . ty "text" $
      [ "op"   .= op
      , "word" .=
      ]
    QContext cs qs     -> object . ty "context" $
      [ "contexts" .= cs
      , "queries"  .= qs
      ]
    QRange l u         -> object . ty "range" $
      [ "lower" .= l ]
      [ "upper" .= u ]
    QNegation q        -> object . ty "not" $
      [ "query" .= q ]
    QQBiary op q1 q2 -> object . ty' op  $
      [ "query1" .= q1
      , "query2" .= q2 ]
    where
    ty' t = (:) ("type" .= t)
    ty  t = ty' (t :: Text)
--}

instance FromJSON Query where
  parseJSON (Object o) = do
    t <- o .: "type"
    case (t :: Text) of
      "wd"  -> o .: "str" >>= return . (QText Case)
      "pi"  -> o .: "str" >>= return . Phrase
      "pc"  -> o .: "str" >>= return . CasePhrase
      "cx"  -> do
        c <- o .: "str"
        q <- o .: "qry"
        return $ Specifier c q
      "and" -> bin And
      "or"  -> bin Or
      "but" -> bin But
      _         -> mzero
    where
    bin op = do
      qs <- o .: "qrys"
      return $ QBinary op qs
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
  put (QText op s)       = put (0 :: Word8) >> put op >> put s
  put (Phrase s)         = put (1 :: Word8) >> put s
  put (CasePhrase s)     = put (3 :: Word8) >> put s
  put (Specifier c q)    = put (5 :: Word8) >> put c >> put q
  put (Negation q)       = put (6 :: Word8) >> put q
  put (QBinary o qs)     = put (7 :: Word8) >> put o >> put qs

  get = do tag <- getWord8
           case tag of
             0 -> liftM2 QText      get get
             1 -> liftM  Phrase     get
             3 -> liftM  CasePhrase get
             5 -> liftM2 Specifier  get get
             6 -> liftM  Negation   get
             7 -> liftM2 QBinary    get get
             _ -> fail "Error while decoding Query"

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

-- FIXME: refactor 'optimize' for lists
optimize :: Query -> Query
optimize = id

{-
-- | Transforms all @(QBinary And q1 q2)@ where one of @q1@ or @q2@ is a @Negation@ into
-- @QBinary Filter q1 q2@ or @QBinary Filter q2 q1@ respectively.
optimize :: Query -> Query

optimize q@(QBinary And (QText NoCase q1) (QText NoCase q2))
  | T.toLower q1 `T.isPrefixOf` T.toLower q2 = QText NoCase q2
  | T.toLower q2 `T.isPrefixOf` T.toLower q1 = QText NoCase q1
  | otherwise = q

optimize q@(QBinary And (QText Case q1) (QText Case q2))
  | q1 `T.isPrefixOf` q2 = QText Case q2
  | q2 `T.isPrefixOf` q1 = QText Case q1
  | otherwise = q

optimize q@(QBinary Or (QText NoCase q1) (QText NoCase q2))
  | T.toLower q1 `T.isPrefixOf` T.toLower q2 = QText NoCase q1
  | T.toLower q2 `T.isPrefixOf` T.toLower q1 = QText NoCase q2
  | otherwise = q

optimize q@(QBinary Or (QText Case q1) (QText Case q2))
  | q1 `T.isPrefixOf` q2 = QText Case q1
  | q2 `T.isPrefixOf` q1 = QText Case q2
  | otherwise = q

optimize (QBinary And q1 (Negation q2))  = QBinary But (optimize q1) (optimize q2)
optimize (QBinary And (Negation q1) q2)  = QBinary But (optimize q2) (optimize q1)

optimize (QBinary And q1 q2)             = QBinary And (optimize q1) (optimize q2)
optimize (QBinary Or q1 q2)              = QBinary Or (optimize q1) (optimize q2)
optimize (QBinary But q1 q2)             = QBinary But (optimize q1) (optimize q2)
optimize (Negation q)                     = Negation (optimize q)
optimize (Specifier cs q)                 = Specifier cs (optimize q)

optimize q                                = q

-- | Check if the query arguments comply with some custom predicate.
checkWith                         :: (Text -> Bool) -> Query -> Bool
checkWith f (QText NoCase s)      = f s
checkWith f (Phrase s)            = f s
checkWith f (QText Case s)        = f s
checkWith f (CasePhrase s)        = f s
checkWith f (QText Fuzzy s)       = f s
checkWith f (Negation q)          = checkWith f q
checkWith f (QBinary _ q1 q2)     = checkWith f q1 && checkWith f q2
checkWith f (Specifier _ q)       = checkWith f q

-- | Returns a list of all terms in the query.
extractTerms                      :: Query -> [Text]
extractTerms (QText NoCase s)     = [s]
extractTerms (QText Case s)       = [s]
extractTerms (QText Fuzzy s)      = [s]
extractTerms (Specifier _ q)      = extractTerms q
extractTerms (Negation q)         = extractTerms q
extractTerms (QBinary _ q1 q2)    = extractTerms q1 ++ extractTerms q2
extractTerms _                    = []
-}
