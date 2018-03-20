module Fox.Types.TextSearchOp where

import qualified Fox.Types.Token as Token

import qualified Data.Text as Text

data TextSearchOp
  = Case | NoCase | PrefixCase | PrefixNoCase
  deriving (Eq, Show)

prefixSearchOp :: TextSearchOp -> Bool
prefixSearchOp op =
  case op of
    PrefixCase   -> True
    PrefixNoCase -> True
    _            -> False

data Match
  = Smaller | Matches | Larger

matches
  :: TextSearchOp
  -> Token.Term
  -> Token.Term
  -> Match
matches op term1 term2 =
  case op of
    Case ->
      case compare term1 term2 of
        LT -> Smaller
        EQ -> Matches
        GT -> Larger

    NoCase ->
      case compare (Text.toCaseFold term1) (Text.toCaseFold term2) of
        LT -> Smaller
        EQ -> Matches
        GT -> Larger

    PrefixCase ->
      case Token.commonPrefixes term1 term2 of
        Just _  -> Matches
        Nothing -> matches Case term1 term2

    PrefixNoCase ->
      case Token.commonPrefixes (Text.toCaseFold term1) (Text.toCaseFold term2) of
        Just _  -> Matches
        Nothing -> matches NoCase term1 term2
