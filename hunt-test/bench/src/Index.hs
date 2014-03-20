{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Numeric
import           Prelude                                     as P

import           GHC.Stats
import           GHC.Stats.Json                              ()
import           System.Environment
import           System.FilePath
import           System.Mem
import           System.Posix.Process
import           System.Process

--import           Control.Applicative
--import           Control.Concurrent
import           Control.Monad

import qualified Data.Binary                                 as Bin
--import           Data.Map                                    (Map)
import qualified Data.Map                                    as M
--import           Data.Maybe
import           Data.Monoid
import           Data.Text                                   (Text)
import qualified Data.Text                                   as T
import           Data.Time.Clock
import qualified Data.Traversable                            as T
import           Data.Typeable

import           Hunt.Common

import qualified Hunt.Index                                  as Ix
import qualified Hunt.Index.InvertedIndex                    as InvIx
import qualified Hunt.Index.PrefixTreeIndex                  as PIx

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
--import qualified Data.ByteString                             as BS
import qualified Data.ByteString.Lazy                        as BL
import qualified Data.ByteString.Lazy.Char8                  as B8

import           Hunt.Common.ApiDocument
import qualified Hunt.Common.Occurrences                     as Occ
import           Hunt.Index.Schema.Analyze

import qualified Hunt.Common.Occurrences.Compression.BZip    as ZB
import qualified Hunt.Common.Occurrences.Compression.Simple9 as Z9
import qualified Hunt.Common.Occurrences.Compression.Snappy  as ZS


import qualified Hunt.Index.ComprPrefixTreeIndex             as CPIx

import           Hunt.Utility

-- ----------------------------------------------------------------------------

data DataSet = DataSet
  { tdName   :: String
  , tdType   :: FileType
  , tdPath   :: FilePath -- the path needs an extension!
  , tdSchema :: Schema
  }
  deriving (Show)

data FileType = Binary | Json
  deriving (Show)

-- ----------------------------------------------------------------------------
-- datasets

dsJokes, dsRandom :: DataSet

-- NOTE: path need an extension!
dsJokes = DataSet
  { tdName   = "jokes"
  , tdType   = Json
  , tdPath   = "../data/jokes/FussballerSprueche.js"
  , tdSchema = schemaJokes
  }

dsRandom = DataSet
  { tdName   = "random"
  , tdType   = Json
  , tdPath   = "../data/random/RandomData.js"
  , tdSchema = schemaRandom
  }

--dataSets :: Map String DataSet
--dataSets = M.fromList . map (\x -> (tdName x, x)) $ [dsJokes, dsRandom]

dataSets, dataSetsJs, dataSetsBin :: [DataSet]
dataSetsJs = [dsJokes, dsRandom]

dataSetsBin = map (transType Binary) dataSetsJs

dataSets = dataSetsJs ++ dataSetsBin

-- ----------------------------------------------------------------------------

fileTypeExt :: FileType -> String
fileTypeExt o = case o of
  Json   -> "js"
  Binary -> "bin"

transType :: FileType -> DataSet -> DataSet
transType ft ds = ds
  { tdName   = tdName ds ++ ex
  , tdType   = ft
  , tdPath   = dropExt (tdPath ds) ++ ex
  }
  where
  ex   = '.':fileTypeExt ft

mkDataSet :: FilePath -> DataSet
mkDataSet path = DataSet
  { tdName   = takeBaseName path
  , tdType   = ty
  , tdPath   = path
  , tdSchema = schemaAll
  }
  where
  ty = case takeExtension path of
    ".bin" -> Binary
    ".js"  -> Json
    _      -> error "mkDataSet: invalid extension"


-- ----------------------------------------------------------------------------
-- IO

getJson :: (FromJSON a) => FilePath -> IO a
getJson file = do
  content <- BL.readFile file
  return . fromRight . eitherDecode $ content

decodeFile :: FilePath -> IO ApiDocuments
decodeFile = Bin.decodeFile
--decodeFile x = fmap (Bin.decode . BL.fromChunks . return) $ BS.readFile x

getDataSet :: DataSet -> IO ApiDocuments
getDataSet ds = do
  x <- rf $ tdPath ds
  return $! x
  where
  rf = case tdType ds of
    Binary -> decodeFile
    Json   -> getJson

-- ----------------------------------------------------------------------------
-- Schema

schemaAll, schemaJokes, schemaRandom :: Schema

schemaJokes = M.fromList $ zip
  [ "wer", "was", "wo", "gruppe" ]
  (repeat cSchemaDefault)

schemaRandom = M.fromList $ zip
  [ "id", "context1", "context2", "contextdate", "contextgeo", "contextint" ]
  (repeat cSchemaDefault)

schemaAll = M.unions [schemaJokes, schemaRandom]


cSchemaDefault :: ContextSchema
cSchemaDefault = ContextSchema
  { cxRegEx      = Just "\\w*"
  , cxNormalizer = []
  , cxWeight     = 1
  , cxDefault    = True
  , cxType       = ctText
  }

-- ----------------------------------------------------------------------------
-- Index

type TextIndex i v = (Typeable (i v), Ix.Index i, Ix.ICon i v, Ix.IVal i v ~ Occurrences)
type TextIndexSK i v = (TextIndex i v, Ix.IKey i v ~ String)
type TextIndexTK i v = (TextIndex i v, Ix.IKey i v ~ Text)


addWordsIx :: (Monad m, TextIndex i v) => (Text -> Ix.IKey i v) -> Words -> DocId -> i v -> m (i v)
addWordsIx keyConv wrds dId i
  = foldrWithKeyM (\_c wl acc ->
      foldrWithKeyM (\w ps acc' ->
        return $ let ck = keyConv w in ck `seq` Ix.insert (keyConv w) (mkOccs dId ps) acc')
      acc wl)
      i wrds
  where
  mkOccs            :: DocId -> [Position] -> Occurrences
  mkOccs did pl     = positionsIntoOccs did pl Occ.empty

  positionsIntoOccs :: DocId -> [Position] -> Occurrences -> Occurrences
  positionsIntoOccs docId ws os = P.foldr (Occ.insert docId) os ws

-- ----------------------------------------------------------------------------
-- index types

ix0 :: PIx.DmPrefixTree Positions
ix0 = Ix.empty
ix1 :: CPIx.ComprOccPrefixTree Z9.CompressedOccurrences
ix1 = Ix.empty
ix2 :: CPIx.ComprOccPrefixTree ZB.CompressedOccurrences
ix2 = Ix.empty
ix3 :: CPIx.ComprOccPrefixTree ZS.CompressedOccurrences
ix3 = Ix.empty
ix4 :: InvIx.InvertedIndex Positions
ix4 = Ix.empty

-- ----------------------------------------------------------------------------
-- existentially quantified container
-- XXX: not sure if this is a good idea

data IndexAll = forall i v. TextIndex i v => IndexAll (i v) (Text -> Ix.IKey i v)

indexes :: [IndexAll]
indexes =
  [ IndexAll ix0 T.unpack
  , IndexAll ix1 T.unpack
  , IndexAll ix2 T.unpack
  , IndexAll ix3 T.unpack
  , IndexAll ix4 id
  ]

-- ----------------------------------------------------------------------------
-- Test

typeName :: Typeable a => a -> String
typeName x
  = (tyConModule . fst . splitTyConApp $ t) ++ "." ++ show t
    where
    t = typeOf x

testIndex :: DataSet -> IndexAll -> IO ()
testIndex dataSet (IndexAll ix f) = do
  !apiDocs  <- getDataSet dataSet :: IO ApiDocuments
  --threadDelay (5 * 10^6)
  let docs = P.map (toDocAndWords' (tdSchema dataSet)) apiDocs
  start <- getCurrentTime
  ix'   <- foldM' (\i ((_d,w),did) -> addWordsIx f w did i) ix (zip docs [0..])
  end <- ix' `seq` getCurrentTime

  -- output
  ix' `seq` printStats Nothing ix' (diffUTCTime end start)

-- ----------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  [dsPath,n] <- getArgs
  -- XXX: cycling through without restart leads to inaccurate results
  --mapM_ (\(ds, ix) -> testIndex ds ix) $ zip (repeat dsRandom) (reverse indexes)
  testIndex
    (mkDataSet dsPath)
    (indexes !! read n)
  --threadDelay (5 * 10^6)


-- ----------------------------------------------------------------------------
-- Utils

showF :: RealFloat a => a -> String
showF f = showFFloat (Just 2) f ""

printStats :: Typeable a => Maybe String -> a -> NominalDiffTime -> IO ()
printStats titleM x tdiff = do
  x `seq` performGC
  stats <- getGCStats
  sep
  -- who knew?
  _ <- T.sequence . fmap (putStrLn . (++) ">> ") $ titleM
  putStrLn $ ">> " ++ typeName x
  ssep
  putStrLn $ "time: " ++ show tdiff
  ssep
  printTop
  {-
  ssep
  putStrLs $
    [ "max bytes used: "
    , showF $ (fromIntegral (maxBytesUsed stats)) / (1024^2)
    , " MB"]
  -}
  --ssep
  --printGCStats $ stats
  sep
  where
  sep  = putStrLn "==========="
  ssep = putStrLn "-----------"
  putStrLs = putStrLn . concat
  printGCStats :: ToJSON a => a -> IO ()
  printGCStats = B8.putStrLn . encodePretty' config
    where
    config = Config
      { confIndent  = 2
      , confCompare = keyOrder ["currentBytesUsed", "maxBytesUsed", "peakMegabytesAllocated"]
                        `mappend` compare
      }

printTop :: IO ()
printTop = do
  pid <- getProcessID
  res <- readProcess "top" ["-cbn", "1" ,"-p", show pid] []
  putStrLn res

-- XXX: hack
dropExt :: FilePath -> FilePath
dropExt path
  = if '/' `elem` rExt
    then path
    else reverse $ tail' rBase
  where
  (rExt,rBase) = break (=='.') $ reverse path
  tail' xs = if null xs then xs else tail xs
