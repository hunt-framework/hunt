module Fox.Index.InvertedFile.String where

import qualified Fox.IO.Read as Read

import qualified Data.Text as Text
import qualified Data.Text.Foreign as Text
import qualified Foreign.Ptr as Ptr

import Prelude hiding (String)

newtype String = String Read.UTF16

toText :: Text.Text -> String -> IO Text.Text
toText prefix (String (Read.UTF16 op n)) = do
  s <- Text.fromPtr (Ptr.castPtr op) (fromIntegral (n `div` 2))
  return (prefix `Text.append` s)
