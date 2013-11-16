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
  , checkWith
  , extractTerms
  )
where

import           Control.Monad

import           Data.Aeson
import           Data.Binary
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Text.Binary      ()

import           Holumbus.Common.BasicTypes (Context, TextSearchOp)
import           Holumbus.Common.Schema

-- ----------------------------------------------------------------------------
data Query = Word       Text              -- ^ Single case-insensitive word.
           | Phrase     Text              -- ^ Single case-insensitive phrase.
           | CaseWord   Text              -- ^ Single case-sensitive word.
           | CasePhrase Text              -- ^ Single case-sensitive phrase.
           | FuzzyWord  Text              -- ^ Single fuzzy word.
           | Specifier  [Context] Query   -- ^ Restrict query to a list of contexts.
           | Negation   Query             -- ^ Negate the query.
           | BinQuery   BinOp Query Query -- ^ Combine two queries through a binary operation.
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
           deriving (Eq, Show)

-- ----------------------------------------------------------------------------
instance ToJSON Query where
  toJSON o = case o of
    Word s            -> object . ty "wi" $ [ "str" .= s ]
    Phrase s          -> object . ty "pi" $ [ "str" .= s ]
    CaseWord s        -> object . ty "wc" $ [ "str" .= s ]
    CasePhrase s      -> object . ty "pc" $ [ "str" .= s ]
    FuzzyWord s       -> object . ty "fz" $ [ "str" .= s ]
    Specifier c q     -> object . ty "cx" $
      [ "str" .= c
      , "qry" .= q]
    Negation q        -> object . ty "nt" $ [ "str" .= q ]
    BinQuery op q1 q2 -> object . ty' op  $
      [ "qry1" .= q1
      , "qry2" .= q2 ]
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
    QBinQuery op q1 q2 -> object . ty' op  $
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
      "text"  -> o .: "text" >>= return . Word
      "pi"  -> o .: "text" >>= return . Phrase
      "wc"  -> o .: "text" >>= return . CaseWord
      "pc"  -> o .: "text" >>= return . CasePhrase
      "fz"  -> o .: "text" >>= return . FuzzyWord
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
      q1 <- o .: "qry1"
      q2 <- o .: "qry2"
      return $ BinQuery op q1 q2
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
  put (Word s)           = put (0 :: Word8) >> put s
  put (Phrase s)         = put (1 :: Word8) >> put s
  put (CaseWord s)       = put (2 :: Word8) >> put s
  put (CasePhrase s)     = put (3 :: Word8) >> put s
  put (FuzzyWord s)      = put (4 :: Word8) >> put s
  put (Specifier c q)    = put (5 :: Word8) >> put c >> put q
  put (Negation q)       = put (6 :: Word8) >> put q
  put (BinQuery o q1 q2) = put (7 :: Word8) >> put o >> put q1 >> put q2

  get = do tag <- getWord8
           case tag of
             0 -> liftM  Word       get
             1 -> liftM  Phrase     get
             2 -> liftM  CaseWord   get
             3 -> liftM  CasePhrase get
             4 -> liftM  FuzzyWord  get
             5 -> liftM2 Specifier  get get
             6 -> liftM  Negation   get
             7 -> liftM3 BinQuery   get get get
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

-- | Transforms all @(BinQuery And q1 q2)@ where one of @q1@ or @q2@ is a @Negation@ into
-- @BinQuery Filter q1 q2@ or @BinQuery Filter q2 q1@ respectively.
optimize :: Query -> Query

optimize q@(BinQuery And (Word q1) (Word q2))
  | T.toLower q1 `T.isPrefixOf` T.toLower q2 = Word q2
  | T.toLower q2 `T.isPrefixOf` T.toLower q1 = Word q1
  | otherwise = q

optimize q@(BinQuery And (CaseWord q1) (CaseWord q2))
  | q1 `T.isPrefixOf` q2 = CaseWord q2
  | q2 `T.isPrefixOf` q1 = CaseWord q1
  | otherwise = q

optimize q@(BinQuery Or (Word q1) (Word q2))
  | T.toLower q1 `T.isPrefixOf` T.toLower q2 = Word q1
  | T.toLower q2 `T.isPrefixOf` T.toLower q1 = Word q2
  | otherwise = q

optimize q@(BinQuery Or (CaseWord q1) (CaseWord q2))
  | q1 `T.isPrefixOf` q2 = CaseWord q1
  | q2 `T.isPrefixOf` q1 = CaseWord q2
  | otherwise = q

optimize (BinQuery And q1 (Negation q2))  = BinQuery But (optimize q1) (optimize q2)
optimize (BinQuery And (Negation q1) q2)  = BinQuery But (optimize q2) (optimize q1)

optimize (BinQuery And q1 q2)             = BinQuery And (optimize q1) (optimize q2)
optimize (BinQuery Or q1 q2)              = BinQuery Or (optimize q1) (optimize q2)
optimize (BinQuery But q1 q2)             = BinQuery But (optimize q1) (optimize q2)
optimize (Negation q)                     = Negation (optimize q)
optimize (Specifier cs q)                 = Specifier cs (optimize q)

optimize q                                = q

-- | Check if the query arguments comply with some custom predicate.
checkWith                         :: (Text -> Bool) -> Query -> Bool
checkWith f (Word s)              = f s
checkWith f (Phrase s)            = f s
checkWith f (CaseWord s)          = f s
checkWith f (CasePhrase s)        = f s
checkWith f (FuzzyWord s)         = f s
checkWith f (Negation q)          = checkWith f q
checkWith f (BinQuery _ q1 q2)    = checkWith f q1 && checkWith f q2
checkWith f (Specifier _ q)       = checkWith f q

-- | Returns a list of all terms in the query.
extractTerms                      :: Query -> [Text]
extractTerms (Word s)             = [s]
extractTerms (CaseWord s)         = [s]
extractTerms (FuzzyWord s)        = [s]
extractTerms (Specifier _ q)      = extractTerms q
extractTerms (Negation q)         = extractTerms q
extractTerms (BinQuery _ q1 q2)   = extractTerms q1 ++ extractTerms q2
extractTerms _                    = []
