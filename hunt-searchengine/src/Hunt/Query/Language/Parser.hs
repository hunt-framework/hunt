{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC  -fno-warn-unused-do-bind #-}

-- ----------------------------------------------------------------------------
{- |
  Module     : Hunt.Query.Language.Parser
  Copyright  : Copyright (C) 2007, 2008 Timo B. Huebel
  License    : MIT

  Maintainer : Timo B. Huebel (tbh@holumbus.org)
  Stability  : experimental
  Portability: portable
  Version    : 0.2

  The Hunt query parser, based on the famous Parsec library.

  The parser implements a default syntax for the query grammar which exposes
  all possible query types and operators to the user.

  Syntax:

  [@AND@, @OR@, @AND NOT@] = combinatory queries

  [@!w@]                   = case sensitive prefix query e.g.: @!car@ or @!Car@

  [@~w@]                   = fuzzy word query            e.g.: @~car@ or @~cra@

  [@\"...\"@]              = phrase query, performs an exact search for a single word

  [@(...)@]                = brackets

  [@c:w@]                  = context sensitive queries   e.g.: @(who:Rudi Voeller)@

  [@c1,c2:w@]              = multi context queries       e.g.: @(content,who,title:Rudi Voeller)@

  [@\[... TO ...\]@]       = range queries               e.g.: @[2014-02-10 TO 2012-02-16]@

  [@w\^b@]                 = query boosting              e.g.: @toList OR toAscList^1.5@
-}
-- ----------------------------------------------------------------------------

module Hunt.Query.Language.Parser
  (
  -- * Parsing
  parseQuery
  )
where

import           Control.Applicative         hiding (many, (<|>))

import           Data.Text                   (Text)
import qualified Data.Text                   as T

import           Text.Parsec
import           Text.Parsec.String

import           Hunt.ClientInterface
-- import           Hunt.Common.BasicTypes        (mkScore)
import           Hunt.Query.Language.Grammar

-- ------------------------------------------------------------

-- | Parse a query using the default syntax provided by the Hunt framework.
parseQuery :: String -> Either Text Query
parseQuery = result . parse query ""
  where
  result (Left err) = Left (T.pack . show $ err)
  result (Right q)  = Right q

-- | A query may always be surrounded by whitespace
query :: Parser Query
query
    = do spaces
         res <- orQuery'
         spaces >> eof
         return res

orQuery' :: Parser Query
orQuery'
    = do q1 <- andQuery'
         qs <- many (orOp1 >> andQuery')
         return $ qOrs (q1 : qs)
    where
      orOp1
          = try orOp'
          where
            orOp' = spaces >> string "OR" >> spaces1

andQuery' :: Parser Query
andQuery'
    = do q1 <- neighborQuery
         qs <- many $
               do op <- andOp1
                  q  <- neighborQuery
                  return (op, q)
         return $ foldl (\ res (op', q') -> op' res q') q1 qs
    where
      andOp1
          = try andNotOp'
            <|> try andOp'
          where
            andNotOp'
                = do spaces >> string "AND" >> spaces >> string "NOT" >> spaces1
                     return qAndNot
            andOp' = do spaces >> string "AND" >> spaces1
                        return qAnd


neighborQuery :: Parser Query
neighborQuery
    = do q1 <- contextSeqQuery
         qs <- many $
               do op <- neiOp
                  q  <- contextSeqQuery
                  return (op, q)
         return $ foldl (\ res (op', q') -> op' res q') q1 qs
    where
      neiOp
          = try nextOp
            <|> try nearOp
            <|> try followOp
          where
            nextOp
                = do spaces >> string "++" >> spaces1
                     return qNext
            nearOp
                = do spaces >> string "NEAR" >> spaces
                     d <- read <$> many1 digit
                     spaces1
                     return $ qNear d

            followOp
                = do spaces >> string "FOLLOW" >> spaces
                     d <- read <$> many1 digit
                     spaces1
                     return $ qNear d

contextSeqQuery :: Parser Query
contextSeqQuery
    = do q1 <- contextQuery
         qs <- many $
               try (spaces1 >> contextQuery)
         return $ foldl qAnd q1 qs

-- | Parse a context query.
contextQuery :: Parser Query
contextQuery = try contextQuery' <|> parQuery
  where
  contextQuery' = do cs <- contexts
                     spaces
                     char ':'
                     spaces
                     t <- parQuery
                     tryBoost (QContext cs t)



-- | Parse a query surrounded by parentheses.
parQuery :: Parser Query
parQuery = parQuery' <|> rangeQuery
  where
  parQuery' = do char '('
                 spaces
                 q <- orQuery'
                 spaces
                 char ')'
                 tryBoost q

-- | Parse a range query.
rangeQuery :: Parser Query
rangeQuery = rangeQuery' <|> caseQuery
  where
  rangeQuery' = do char '['
                   spaces
                   l <- word
                   spaces1
                   string "TO"
                   spaces1
                   u <- word
                   spaces
                   char ']'
                   tryBoost $ QRange (T.pack l) (T.pack u)

-- | Parse a case-sensitive query.
caseQuery :: Parser Query
caseQuery = caseQuery' <|> fuzzyQuery
  where
  caseQuery' = do char '!'
                  spaces
                  phraseQuery qPhrase <|> wordQuery qWord

-- | Parse a fuzzy query.
fuzzyQuery :: Parser Query
fuzzyQuery
    = fuzzyQuery'
      <|> phraseQuery qPhraseNoCase
      <|> wordQuery qPrefixPhraseNoCase
  where
  fuzzyQuery' = do char '~'
                   spaces
                   wordQuery (setFuzzySearch . qWord)

-- | Parse a word query.
wordQuery :: (Text -> Query) -> Parser Query
wordQuery c = do
              w <- word
              tryBoost (c $ T.pack w)

-- | Parse a phrase query.
phraseQuery :: (Text -> Query) -> Parser Query
phraseQuery c = do p <- phrase
                   tryBoost (c $ T.pack p)

-- | Parse a word.
word :: Parser String
word = try $
       do w <- many1 (escapedChar <|> wordChar)
          if w `elem` ["OR", "AND", "++", "NEAR", "FOLLOW"]
            then parserZero
            else return w

-- | Parse an escape sequence. @\@ followed by the character, e.g. @\"@.
escapedChar :: Parser Char
escapedChar = char escapeChar *> decodeChar

-- | Parse a single valid escape character, e.g. @"@.
decodeChar :: Parser Char
decodeChar = choice (zipWith decode notWordChar notWordChar)
    where decode c r = r <$ char c

-- | The character an escape sequence starts with.
escapeChar :: Char
escapeChar = '\\'

-- | Parse a phrase.
phrase :: Parser String
phrase = do char '"'
            p <- many1 phraseChar
            char '"'
            return p

-- | Parse a boosted query.
tryBoost :: Query -> Parser Query
tryBoost q = try boost <|> return q
  where
  boost = do
          char '^'
          b <- simplePositiveFloat
          return (QBoost (mkScore b) q)

-- | Parse a character of a word.
wordChar :: Parser Char
wordChar = noneOf notWordChar

-- | Characters that cannot occur in a word (and have to be escaped).
notWordChar :: String
notWordChar = escapeChar : "\")([]^ \n\r\t"

-- | Parse a character of a phrases.
phraseChar :: Parser Char
phraseChar = noneOf "\""

-- | Parse a list of contexts.
contexts :: Parser [Text]
contexts = context `sepBy1` char ','

-- | Parse a context.
context :: Parser Text
context = do spaces
             c <- many1 alphaNum
             spaces
             return (T.pack c)

-- | Parse at least on white space character.
spaces1 :: Parser ()
spaces1 = skipMany1 space

-- | Parse a simple positive number.
simplePositiveNumber :: Parser String
simplePositiveNumber = many1 digit

-- | Parse a simple positive float. The decimal point with following numbers is optional.
simplePositiveFloat :: Parser Float
simplePositiveFloat = fmap read $ simplePositiveNumber <++> decimal
  where decimal  = option "" $ char '.' <:> simplePositiveNumber

-- ------------------------------------------------------------

-- |Applicative concat @(++)@.
(<++>) :: Applicative f => f [a] -> f [a] -> f [a]
(<++>) a b = (++) <$> a <*> b

-- Applicative cons @(:)@.
(<:>) :: Applicative f => f a -> f [a] -> f [a]
(<:>) a b = (:) <$> a <*> b

-- ------------------------------------------------------------
