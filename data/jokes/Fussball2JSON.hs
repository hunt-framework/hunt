{-# LANGUAGE OverloadedStrings #-}

module FussballToJSON where
import           Control.Monad               (mzero)

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
type Content      = Text
type Position     = Word32
type Context      = Text
type Description  = Map Text Text

-- | Positions of Words for each context.
type Words        = Map Context WordList

-- | Positions of words in the document.
type WordList     = Map Word [Position]

-- | Multiple ApiDocuments.
type ApiDocuments = [ApiDocument]

-- | The document accepted via the API.
data ApiDocument  = ApiDocument
  { apiDocUri       :: URI
  , apiDocIndexMap  :: Map Context IndexData
  , apiDocDescrMap  :: Description
  }

-- | Data necessary for adding documents to the index.
data IndexData = IndexData
  { idContent       :: Content
  , idMetadata      :: IndexMetadata
  }

-- | Metadata for index processing
data IndexMetadata = IndexMetadata
  { imAnalyzer :: AnalyzerType
  } deriving Eq

-- | Text analysis function
type AnalyzerFunction = Text -> [(Position, Text)]

-- | Types of analyzer
data AnalyzerType
  = DefaultAnalyzer
  deriving Eq


  -- | The default Matadata
defaultIndexMetadata :: IndexMetadata
defaultIndexMetadata = IndexMetadata
  { imAnalyzer = DefaultAnalyzer
  }

-- | empty document
emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" M.empty M.empty

instance FromJSON ApiDocument where
  parseJSON (Object o) = do
    parsedUri         <- o    .: "uri"
    indexMap          <- o    .: "index"
    descrMap          <- o    .: "description"
    return ApiDocument
      { apiDocUri       = parsedUri
      , apiDocIndexMap  = indexMap
      , apiDocDescrMap  = descrMap
      }
  parseJSON _ = mzero


instance FromJSON IndexData where
  parseJSON (Object o) = do
    content           <- o    .:  "content"
    metadata          <- o    .:? "metadata" .!= defaultIndexMetadata
    return IndexData
      { idContent       = content
      , idMetadata      = metadata
      }
  parseJSON _ = mzero


instance FromJSON IndexMetadata where
  parseJSON (Object o) = do
    analyzer <- o .: "analyzer" .!= DefaultAnalyzer
    return IndexMetadata
      { imAnalyzer = analyzer
      }
  parseJSON _ = mzero


instance FromJSON AnalyzerType where
  parseJSON (String s) =
    case s of
      "default" -> return DefaultAnalyzer
      _         -> mzero
  parseJSON _ = mzero



instance ToJSON ApiDocument where
  toJSON (ApiDocument u im dm) = object
    [ "uri"         .= u
    , "index"       .= im
    , "description" .= dm
    ]

instance ToJSON IndexData where
  toJSON (IndexData c m) = object $
    "content"     .= c
      : if m == defaultIndexMetadata
        then []
        else [ "metadata"    .= m]


instance ToJSON IndexMetadata where
  toJSON (IndexMetadata a) = object
    [ "analyzer"    .= a
    ]

instance ToJSON AnalyzerType where
  toJSON (DefaultAnalyzer) =
    "default"


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


joke2Api :: Joke -> ApiDocument
joke2Api (n, who, what, wher, grp)
    = ApiDocument
      { apiDocUri
          = T.pack . ("joke://joke" ++) . show $ n
      , apiDocDescrMap
          = descrMap
      , apiDocIndexMap = indexMap
      }
    where
    defaultMetadata = IndexMetadata DefaultAnalyzer
    descrMap = M.fromList $
      (if null wher
      then []
      else [("wo", T.pack wher)])
      ++
      [ ("wer",    T.pack who)
      , ("was",    T.pack what)
      , ("gruppe", T.pack grp)
      ]
    indexMap = M.map (\text -> IndexData text defaultMetadata) descrMap

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