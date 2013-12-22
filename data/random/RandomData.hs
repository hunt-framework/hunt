{-# LANGUAGE OverloadedStrings #-}
module RandomData
where

import           Control.Monad            (mzero)

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy     as B
import           Data.Map                 (Map ())
import qualified Data.Map                 as M
import           Data.Monoid              (mappend)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Word                (Word32)
import           Data.Maybe               (fromMaybe)

import           System.IO

import           Test.QuickCheck
import           Test.QuickCheck.Gen

import           System.Environment       (getArgs)
import           System.Random

import           Text.Lorem.Words
import           Data.Time
import           Data.Time.Format
import           System.Locale

-- ------------------------------------------------------------

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

-- | The document accepted via the API.
data ApiDocument  = ApiDocument
  { apiDocUri       :: URI
  , apiDocIndexMap  :: Map Context Text
  , apiDocDescrMap  :: Description
  } deriving (Eq, Show)

-- | Data necessary for adding documents to the index.
data IndexData = IndexData
  { idContent       :: Content
  , idMetadata      :: IndexMetadata
  } deriving (Eq, Show)

-- | Metadata for index processing
data IndexMetadata = IndexMetadata
  { imAnalyzer :: AnalyzerType
  } deriving (Eq, Show)

-- | Text analysis function
type AnalyzerFunction = Text -> [(Position, Text)]

-- | Types of analyzer
data AnalyzerType
  = DefaultAnalyzer
  deriving (Eq, Show)


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

-- ------------------------------------------------------------

apiDocs :: Int -> Int -> IO [ApiDocument]
apiDocs = mkData apiDocGen


mkData :: (Int -> Gen a) -> Int -> Int -> IO [a]
mkData gen minS maxS =
  do rnd0 <- newStdGen
     let rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = split rnd
     return [unGen (gen i) r n | ((r,n),i) <- rnds rnd0 `zip` cycle [minS..maxS] `zip` [1..]] -- simple cycle


apiDocGen :: Int -> Gen ApiDocument
apiDocGen n = do
  desc    <- descriptionGen
  ix      <- mkIndexData n desc
  return  $ ApiDocument uri ix desc
  where uri = T.pack . ("rnd://" ++) . show $ n

niceText1 :: Gen Text
niceText1 = fmap T.pack . listOf1 . elements $ concat [" ", ['0'..'9'], ['A'..'Z'], ['a'..'z']]


loremText :: Gen Text
loremText = do
  words <- listOf1 $ elements latin
  return $ T.intercalate " " words

descriptionGen :: Gen Description
descriptionGen = do
  tuples <- vectorOf 3 kvTuples
  dates  <- cxDate
  return $ M.fromList (dates:tuples)
  where
  kvTuples = do
    a <- resize 15 niceText1 -- keys are short
    b <- loremText
    return (a,b)
  cxDate = do
    ds <- vectorOf 3 date >>= \l -> return $ T.intercalate " " l
    return ("dates", ds)

date :: Gen Text
date = arbitrary >>= \x -> return . T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" (newDate x)
  where
  newDate x = addDays (-x) (fromGregorian 2013 12 31)

mkIndexData :: Int -> Description -> Gen (Map Context Text)
mkIndexData i d = do
  return $ M.fromList [ ("id", index)
                      , ("context1", cx1)
                      , ("context2", cx2)
                      , ("contextdate", cxd)
                      ]
  where
  index   = T.pack $ show i
  cx1 = T.intercalate " " . map (T.take 4 . T.filter (/=' ') . snd) . M.toList $ d
  cx2 = T.intercalate " " . map snd $  M.toList  d
  cxd = fromMaybe "" (M.lookup "dates" d)

-- ------------------------------------------------------------

encodeDocs :: [ApiDocument] -> B.ByteString
encodeDocs
    = encodePretty' encConfig
      where
        encConfig :: Config
        encConfig
            = Config { confIndent = 2
                     , confCompare
                         = keyOrder ["uri", "description", "index", "content"]
                           `mappend`
                           compare
                     }


run :: Int -> Int -> Int -> IO ()
run minS maxS numD = do
  h <- openFile "RandomData.js" WriteMode
  docs <- apiDocs minS maxS
  B.hPutStr h . encodeDocs $ take numD docs
  hPutStrLn h ""
  hClose h
  return ()


main :: IO ()
main = do
  args <- getArgs
  case map read args of
    []                  -> run minS maxS numD
    [aNum]              -> run minS maxS aNum
    [aNum, aMax]        -> run minS aMax aNum
    [aNum, aMax, aMin]  -> run aMin aMax aNum
  where
  minS = 200
  maxS = 200
  numD = 1000
