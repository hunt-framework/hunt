{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad            (mzero)
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
import           Prelude                  hiding (Word)
import           System.IO

-- ----------------------------------------------------------------------------

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

-- ----------------------------------------------------------------------------

-- | Multiple ApiDocuments.
type ApiDocuments = [ApiDocument]

-- | The document accepted via the API.
data ApiDocument  = ApiDocument
  { apiDocUri      :: URI
  , apiDocIndexMap :: Map Context Content
  , apiDocDescrMap :: Description
  }
  deriving (Show)

-- | Text analysis function
type AnalyzerFunction = Text -> [(Position, Text)]

-- | Types of analyzer
data AnalyzerType
  = DefaultAnalyzer
  deriving (Show)

-- | paged api document result
data LimitedResult x = LimitedResult
  { lrResult :: [x]
  , lrOffset :: Int
  , lrMax    :: Int
  , lrCount  :: Int
  }
  deriving (Show, Eq)

mkLimitedResult :: Int -> Int -> [x] -> LimitedResult x
mkLimitedResult offset mx xs = LimitedResult
  { lrResult = take mx . drop offset $ xs
  , lrOffset = offset
  , lrMax    = mx
  , lrCount  = length xs
  }

-- | empty document
emptyApiDocIndexMap :: Map Context Content
emptyApiDocIndexMap = M.empty

emptyApiDocDescrMap :: Description
emptyApiDocDescrMap = M.empty

emptyApiDoc :: ApiDocument
emptyApiDoc = ApiDocument "" emptyApiDocIndexMap emptyApiDocDescrMap

-- ----------------------------------------------------------------------------

instance (ToJSON x) => ToJSON (LimitedResult x) where
   toJSON (LimitedResult res offset mx cnt) = object
    [ "result" .= res
    , "offset" .= offset
    , "max"    .= mx
    , "count"  .= cnt
    ]

instance FromJSON ApiDocument where
  parseJSON (Object o) = do
    parsedUri         <- o    .: "uri"
    indexMap          <- o    .:? "index"       .!= emptyApiDocIndexMap
    descrMap          <- o    .:? "description" .!= emptyApiDocDescrMap
    return ApiDocument
      { apiDocUri       = parsedUri
      , apiDocIndexMap  = indexMap
      , apiDocDescrMap  = descrMap
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

instance ToJSON AnalyzerType where
  toJSON (DefaultAnalyzer) =
    "default"

-- ----------------------------------------------------------------------------

joke2Api :: Joke -> ApiDocument
joke2Api (n, who, what, wher, grp)
    = ApiDocument
      { apiDocUri
          = T.pack . ("joke://joke" ++) . show $ n
      , apiDocDescrMap = descrMap
      , apiDocIndexMap = indexMap
      }
    where
    descrMap = M.fromList $
      (if null wher
      then []
      else [("wo", T.pack wher)])
      ++
      [ ("wer",    T.pack who)
      , ("was",    T.pack what)
      , ("gruppe", T.pack grp)
      ]
    indexMap = descrMap

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
#if MIN_VERSION_aeson_pretty(0, 8, 0)
        indent = Spaces 2
#else
        indent = 2
#endif

        encConfig :: Config
        encConfig
            = Config { confIndent = indent
                     , confCompare
                         = keyOrder ["uri", "desc", "words", "wer","was","wo","gruppe"]
                           `mappend`
                           compare
                     }

-- ----------------------------------------------------------------------------

main :: IO ()
main
    = do h <- openFile "FussballerSprueche.js" WriteMode
         B.hPutStr h . encodeDocs . map joke2Api $ allJokes
         hPutStrLn h ""
         hClose h
         return ()
