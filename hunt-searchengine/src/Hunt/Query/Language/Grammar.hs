{-# LANGUAGE OverloadedStrings #-}

-- ----------------------------------------------------------------------------
{- |
  The query language.

  'Query' specifies the complete grammar.

  "Hunt.Query.Language.Parser" provides a parser for plain text queries.
-}
-- ----------------------------------------------------------------------------

module Hunt.Query.Language.Grammar
    ( -- * Query data types
      Query (..)
    , BinOp (..)
    , TextSearchType (..)
    , escapeChar
    , notWordChar

    -- * Optimizing
    , optimize
    , checkWith
    , extractTerms
    -- * Pretty printing
    , printQuery
    )
where

import           Control.Applicative
import           Control.Monad

import           Data.Aeson
import           Data.Binary
import           Data.Monoid            ((<>))
import           Data.Text              (Text)
import qualified Data.Text              as T

import           Hunt.Common.BasicTypes as BTy
import           Hunt.Scoring.Score     (Score, unScore, toDefScore)

import           Text.Read              (readMaybe)

-- ------------------------------------------------------------

-- TODO: the constructors QPhrase and QBinary can be removed
-- they can be represented by QFullWord and QSeq.
--
-- Currently these operators are transformed during query evaluation
-- on the fly into QFullWord and QSeq.

-- | The query language.
data Query
  = QWord     TextSearchType Text  -- ^ prefix search for a word
  | QFullWord TextSearchType Text  -- ^ search for a complete word
  | QPhrase   TextSearchType Text  -- ^ Phrase search.
  | QContext  [Context] Query      -- ^ Restrict a query to a list of contexts.
  | QBinary   BinOp Query Query    -- ^ Combine two queries with a binary operation.
  | QSeq      BinOp [Query]
  | QBoost    Score  Query         -- ^ Weight for query.
  | QRange    Text Text            -- ^ Range query.
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
  = And        -- ^ Intersect two queries.
  | Or         -- ^ Union two queries.
  | AndNot     -- ^ Filter a query by another.
  | Phrase     -- ^ Search for a sequence of words
  | Follow Int -- ^ Search a word followed another word within a distance
  | Near   Int -- ^ search a word followed or preceded another word within a distance
  deriving (Eq, Show)

-- ------------------------------------------------------------
-- JSON instances
-- ------------------------------------------------------------

instance ToJSON Query where
  toJSON o = case o of
    QWord op w        -> object . ty "word"     $ [ "op" .= op, "word"   .= w ]
    QFullWord op w    -> object . ty "fullword" $ [ "op" .= op, "word"   .= w ]
    QPhrase op s      -> object . ty "phrase"   $ [ "op" .= op, "phrase" .= s ]
    QContext c q      -> object . ty "context"  $ [ "contexts" .= c , "query" .= q ]
    QBinary op q1 q2  -> object . ty' op        $ [ "query1" .= q1, "query2" .= q2 ]
    QSeq    op qs     -> object . ty "seq"      $ [ "op" .= op, "args" .= qs ]
    QBoost  w q       -> object . ty "boost"    $ [ "weight" .= w, "query" .= q ]
    QRange l u        -> object . ty "range"    $ [ "lower" .= l, "upper" .= u ]
    where
    ty' t = (:) ("type" .= t)
    ty  t = ty' (t :: Text)

instance FromJSON Query where
  parseJSON (Object o) = do
    t <- o .: "type"
    case (t :: Text) of
      "word"
          -> QWord     <$> (o .: "op") <*> (o .: "word")
      "fullword"
          -> QFullWord <$> (o .: "op") <*> (o .: "word")
      "phrase"
          -> QPhrase   <$> (o .: "op") <*> (o .: "phrase")
      "context"
          -> QContext  <$> (o .: "contexts") <*> (o .: "query")
      "boost"
          -> QBoost    <$> (o .: "weight") <*> (o .: "query")
      "range"
          -> QRange    <$> (o .: "lower") <*> (o .: "upper")
      "and"
          -> bin And
      "or"
          -> bin Or
      "and not"
          -> bin AndNot
      "seq"
          -> QSeq      <$> (o .: "op") <*> (o .: "args")
      _   -> mzero
    where
    bin op
        = QBinary op   <$> (o .: "query1") <*> (o .: "query2")
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
    And      -> "and"
    Or       -> "or"
    AndNot   -> "and not"
    Phrase   -> "phrase"
    Follow d -> String $ "follow " <> T.pack (show d)
    Near   d -> String $ "near "   <> T.pack (show d)

instance FromJSON BinOp where
  parseJSON (String s)
    = case T.words s of
      ["and"]        -> return And
      ["or"]         -> return Or
      ["and", "not"] -> return AndNot
      ["phrase"]     -> return Phrase
      ["follow", d]  -> maybe mzero (return . Follow) . readMaybe . T.unpack $ d
      ["near", d]    -> maybe mzero (return . Near  ) . readMaybe . T.unpack $ d
      _              -> mzero
  parseJSON _ = mzero

-- ------------------------------------------------------------
-- Binary instances
-- ------------------------------------------------------------

instance Binary Query where
  put (QWord op s)       = put (0 :: Word8) >> put op >> put s
  put (QFullWord op s)   = put (7 :: Word8) >> put op >> put s
  put (QPhrase op s)     = put (1 :: Word8) >> put op >> put s
  put (QContext c q)     = put (2 :: Word8) >> put c  >> put q
  put (QBinary o q1 q2)  = put (4 :: Word8) >> put o  >> put q1 >> put q2
  put (QSeq    o qs)     = put (8 :: Word8) >> put o  >> put qs
  put (QBoost w q)       = put (5 :: Word8) >> put w  >> put q
  put (QRange l u)       = put (6 :: Word8) >> put l  >> put u

  get = do
    tag <- getWord8
    case tag of
      0 -> QWord     <$> get <*> get
      7 -> QFullWord <$> get <*> get
      1 -> QPhrase   <$> get <*> get
      2 -> QContext  <$> get <*> get
      4 -> QBinary   <$> get <*> get <*> get
      8 -> QSeq      <$> get <*> get
      5 -> QBoost    <$> get <*> get
      6 -> QRange    <$> get <*> get
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
  put And        = put (0 :: Word8)
  put Or         = put (1 :: Word8)
  put AndNot     = put (2 :: Word8)
  put Phrase     = put (3 :: Word8)
  put (Follow d) = put (4 :: Word8) >> put d
  put (Near   d) = put (5 :: Word8) >> put d

  get = do
    tag <- getWord8
    case tag of
      0 -> return And
      1 -> return Or
      2 -> return AndNot
      3 -> return Phrase
      4 -> Follow <$> get
      5 -> Near   <$> get
      _ -> fail "Error while decoding BinOp"

-- ------------------------------------------------------------

-- | Characters that cannot occur in a word (and have to be escaped).
notWordChar :: String
notWordChar = escapeChar : "\"')([]^ \n\r\t"

-- | The character an escape sequence starts with.
escapeChar :: Char
escapeChar = '\\'

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
checkWith f (QFullWord _ s)       = f s
checkWith f (QPhrase _ s)         = f s
checkWith f (QBinary _ q1 q2)     = checkWith f q1 && checkWith f q2
checkWith f (QSeq _ qs)           = and $ map (checkWith f) qs
checkWith f (QContext _ q)        = checkWith f q
checkWith f (QBoost _ q)          = checkWith f q
checkWith f (QRange s1 s2)        = f s1 && f s2

-- | Returns a list of all terms in the query.
extractTerms                      :: Query -> [Text]
extractTerms (QWord _ s)           = [s]
extractTerms (QFullWord _ s)       = [s]
extractTerms (QContext _ q)        = extractTerms q
extractTerms (QBinary _ q1 q2)     = extractTerms q1 ++ extractTerms q2
extractTerms _                     = []

-- ------------------------------------------------------------

-- | Renders a text representation of a Query.

printQuery :: Query -> Text
printQuery (QWord QNoCase w)
    = printWord w

printQuery (QWord QCase w)
    = "!" <> printWord w

printQuery (QWord QFuzzy w)
    = "~" <> printWord w

printQuery (QFullWord QNoCase w)
    = printPhrase w

printQuery (QFullWord QCase w)
    = "!" <> printPhrase w

printQuery (QFullWord QFuzzy w)
    = "~" <> printPhrase w

printQuery (QPhrase _ w)
    = printPhrase w

printQuery (QContext [] w)
    = printQPar w

printQuery (QContext cs' w)
    = printCs <> ":(" <> (printQPar w) <> ")"
      where
        printCs = foldr1 (\l r -> l <> "," <> r) cs'

printQuery (QBinary o l r)
    = (printQPar l) <> (printOp o) <> (printQPar r)

printQuery (QSeq _ [])
    = ""
printQuery (QSeq _ [q])
    = printQuery q
printQuery (QSeq o qs)
    = foldr1 (\ res arg -> res <> printOp o <> arg) $
      map printQPar qs

printQuery (QBoost w q)
    = (printQPar q) <> "^" <> (T.pack $ show $ unScore $ toDefScore $ w)

printQuery (QRange l u)
    = "[" <> l <> " TO " <> u <> "]"

printOp :: BinOp -> Text
printOp And        = " " -- the token AND is not required.
printOp Or         = " OR "
printOp AndNot     = " AND NOT "
printOp Phrase     = " ++ "
printOp (Follow d) = " FOLLOW " <> (T.pack $ show d) <> " "
printOp (Near   d) = " NEAR "   <> (T.pack $ show d) <> " "

-- | Maybe render paranthesis.
printQPar :: Query -> Text
printQPar q@QWord{}     = printQuery q
printQPar q@QFullWord{} = printQuery q
printQPar q@QPhrase{}   = printQuery q
printQPar q@QRange{}    = printQuery q
printQPar q@QContext{}  = printQuery q
printQPar q             = "(" <> (printQuery q) <> ")"

printPhrase :: Text -> Text
printPhrase w
    = "\"" <> escapeWord toBeQuoted w <> "\""
    where
      toBeQuoted = (== '\"')

printWord :: Text -> Text
printWord w
    | T.any toBeQuoted w = "'" <> escapeWord (== '\'') w <> "'"
    | otherwise          = w
    where
      toBeQuoted c = elem c $ notWordChar

escapeWord :: (Char -> Bool) -> Text -> Text
escapeWord p t
    = T.concatMap esc t
      where
        esc c
            | p c = T.pack ('\\' : c : [])
            | otherwise = T.singleton c

-- ------------------------------------------------------------
