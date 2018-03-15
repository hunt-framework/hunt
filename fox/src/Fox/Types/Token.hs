module Fox.Types.Token where

import Fox.Types.Positions (Position)

import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text

type Term = Text

-- | A piece of text together with a position.
data Token = Token !Position !Term

null :: Term -> Bool
null = Text.null

empty :: Term
empty = Text.empty

length :: Term -> Int
length = Text.length

lengthWord16 :: Term -> Int
lengthWord16 = Text.lengthWord16

fromText :: Text -> Term
fromText = id

nullToken :: Token -> Bool
nullToken (Token _ term) = Text.null term

-- TODO: rename to commonPrefix
commonPrefixes :: Term -> Term -> Maybe (Term, Term)
commonPrefixes t1 t2 =
  case Text.commonPrefixes t1 t2 of
    Just (prefix, _, suffix) -> Just (prefix, suffix)
    Nothing                  -> Nothing
