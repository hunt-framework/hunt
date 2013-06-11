{-# LANGUAGE OverloadedStrings #-}

module FussballToJSON where
import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as B
import           Data.Char
import           Data.Map                 (Map ())
import qualified Data.Map                 as M
import           Data.Monoid
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Word                (Word32)
import           FussballerSprueche

import           System.IO

type URI          = Text
type Word         = Text
type Position     = Word32
type Context      = Text

-- | map from context to a list of words with occurrences
type Words        = Map Context WordList

-- | map from word to a list of occurrences
type WordList     = Map Word [Position]

type Attribute    = Text
type Description  = Map Attribute Text

-- | type the server receives to add/modify/delete? in index
data ApiDocument = ApiDocument
  { apiDocUri   :: URI
  , apiDocDesc  :: Description
  , apiDocWords :: Words
  } deriving Show

type ApiDocuments = [ApiDocument]

instance ToJSON ApiDocument where
  toJSON (ApiDocument u d ws) = object
    [ "uri"   .= u
    , "desc"  .= toJSON d
    , "words" .= toJSON ws
    ]

joke2Api :: Joke -> ApiDocument
joke2Api (n, who, what, wher, grp)
    = ApiDocument
      { apiDocUri
          = T.pack . ("joke://joke" ++) . show $ n
      , apiDocDesc
          = M.fromList $
            (if null wher
            then []
            else [("wo", T.pack wher)])
            ++
            [ ("wer",    T.pack who)
            , ("was",    T.pack what)
            , ("gruppe", T.pack grp)
            ]
      , apiDocWords
          = M.fromList
            [ ("wer",    toWL who)
            , ("was",    toWL what)
            , ("wo",     toWL wher)
            , ("gruppe", toWL grp)
            ]
      }

toWL :: String -> WordList
toWL = foldr insert M.empty . scanText
    where
      insert :: (Word32, Text) -> WordList -> WordList
      insert (p, w)
          = M.insertWith (++) w [p]

scanText :: String -> [(Word32, Text)]
scanText
    = zip [0..] . map T.pack . words . map ( \ c -> if isAlphaNum c then c else ' ')

encodeDocs :: ApiDocuments -> B.ByteString
encodeDocs
    = encodePretty' encConfig
      where
        encConfig :: Config
        encConfig
            = Config { confIndent = 2
                     , confCompare
                         = keyOrder ["uri", "desc", "words", "wer","was","wo","gruppe"]
                           `mappend`
                           compare
                     }

main :: IO ()
main
    = do h <- openFile "FussballerSprueche.js" WriteMode
         B.hPutStr h . encodeDocs . map joke2Api $ allJokes
         hPutStrLn h ""
         hClose h
         return ()