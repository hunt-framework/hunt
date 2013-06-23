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

-- | The raw content of a document.
type ContentRaw = Text

-- | The document accepted via the API.
data ApiDocument = ApiDocument
  { apiDocUri       :: URI
  , apiDocMapping   :: Map Context Content
  }

data Content = Content
  { contentRaw      :: ContentRaw
  , contentMetadata :: ContentMetadata
  }

-- | Information where the (Context -> Content) mapping is stored.
data ContentMetadata = ContentMetadata
  { indexField    :: Bool -- ^ Should the (Context -> Content) mapping be added to the index?
  , docField      :: Bool -- ^ Should the (Context -> Content) mapping be part of the document description?
  } deriving Eq

-- | The default Attribute Matadata - only add the mapping to the index.
defaultContentMetadata :: ContentMetadata
defaultContentMetadata = ContentMetadata True False

-- | empty document
emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" M.empty

instance ToJSON ApiDocument where
  toJSON (ApiDocument u m) = object
    [ "uri"         .= u
    , "mapping"     .= m
    ]

instance ToJSON Content where
  toJSON (Content c m) = object
    [ "content"     .= c
    , "metadata"    .= if m == defaultContentMetadata then Nothing else Just m
    ]

instance ToJSON ContentMetadata where
  toJSON (ContentMetadata i d) = object
    [ "indexField"  .= i
    , "docField"    .= d
    ]

type ApiDocuments = [ApiDocument]

-- |  some sort of json response format
data JsonResponse r = JsonSuccess r | JsonFailure [Text]

instance (ToJSON r) => ToJSON (JsonResponse r) where
  toJSON (JsonSuccess msg) = object
    [ "code"  .= (0 :: Int)
    , "msg"   .= msg
    ]

  toJSON (JsonFailure msg) = object
    [ "code"  .= (1 :: Int)
    , "msg"   .= msg
    ]

indexMetadata   :: ContentMetadata
indexMetadata   = ContentMetadata True False

docMetadata     :: ContentMetadata
docMetadata     = ContentMetadata False True

bothMetadata    :: ContentMetadata
bothMetadata    = ContentMetadata True True


joke2Api :: Joke -> ApiDocument
joke2Api (n, who, what, wher, grp)
    = ApiDocument
      { apiDocUri
          = T.pack . ("joke://joke" ++) . show $ n
      , apiDocMapping
          = M.fromList $
            (if null wher
            then []
            else [("wo", Content (T.pack wher) bothMetadata)])
            ++
            [ ("wer",    Content (T.pack who)  bothMetadata)
            , ("was",    Content (T.pack what) bothMetadata)
            , ("gruppe", Content (T.pack grp)  bothMetadata)
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