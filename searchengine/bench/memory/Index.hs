{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TypeFamilies              #-}

module Main where

import           Numeric
import           Prelude                                         as P

import           GHC.Stats
import           GHC.Stats.Json                                  ()
import           System.Mem
import           System.Posix.Process
import           System.Process

import           Control.Concurrent

import           Data.Map                                        (Map)
import qualified Data.Map                                        as M
import           Data.Monoid
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T
import           Data.Time.Clock
import qualified Data.Traversable                                as T
import           Data.Typeable

import           Holumbus.Common

import qualified Holumbus.Index.Index                            as Ix
import qualified Holumbus.Index.InvertedIndex                    as InvIx
import qualified Holumbus.Index.PrefixTreeIndex                  as PIx

import           Data.Aeson
import           Data.Aeson.Encode.Pretty
import qualified Data.ByteString.Lazy                            as BL
import qualified Data.ByteString.Lazy.Char8                      as B8
import qualified Data.List                                       as L

import           Holumbus.Common.ApiDocument
import qualified Holumbus.Common.Occurrences                     as Occ
import           Holumbus.Index.Schema.Analyze

import qualified Holumbus.Common.Occurrences.Compression.BZip    as ZB
import qualified Holumbus.Common.Occurrences.Compression.Simple9 as Z9
import qualified Holumbus.Common.Occurrences.Compression.Snappy  as ZS


import qualified Holumbus.Index.ComprPrefixTreeIndex             as CPIx

-- ----------------------------------------------------------------------------

data DataSet = DataSet
  { tdName   :: String
  , tdPath   :: FilePath
  , tdSchema :: Schema
  }

-- ----------------------------------------------------------------------------
-- datasets

dsJokes, dsRandom :: DataSet

dsJokes = DataSet
  { tdName   = "jokes"
  , tdPath   = "../data/jokes/FussballerSprueche.js"
  , tdSchema = schemaJokes
  }

dsRandom = DataSet
  { tdName   = "random"
  , tdPath   = "../data/random/RandomData.js"
  , tdSchema = schemaRandom
  }

dataSets :: Map String DataSet
dataSets = M.fromList . map (\x -> (tdName x, x)) $ [dsJokes, dsRandom]

-- ----------------------------------------------------------------------------
-- IO

getDataSet :: DataSet -> IO ApiDocuments
getDataSet DataSet{..} = do
  content <- BL.readFile tdPath
  return . fromLeft . eitherDecode $ content
  where
  fromLeft (Left msg) = error msg
  fromLeft (Right x)  = x

-- ----------------------------------------------------------------------------
-- Schema

schemaJokes, schemaRandom :: Schema

schemaJokes = M.fromList $ zip
  [ "wer", "was", "wo", "gruppe" ]
  (repeat cSchemaDefault)

schemaRandom = M.fromList $ zip
  [ "id", "context1", "context2", "contextdate", "contextgeo", "contextint" ]
  (repeat cSchemaDefault)

cSchemaDefault :: ContextSchema
cSchemaDefault = ContextSchema
  { cxName       = "default"
  , cxRegEx      = "\\w*"
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


addWordsIx :: TextIndex i v => (Text -> Ix.IKey i v) -> Words -> DocId -> i v -> i v
addWordsIx keyConv wrds dId i
  = M.foldrWithKey (\_c wl acc ->
      M.foldrWithKey (\w ps acc' ->
        let ck = keyConv w in ck `seq` Ix.insert (keyConv w) (mkOccs dId ps) acc')
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
  apiDocs  <- getDataSet dataSet :: IO ApiDocuments
  let docs = P.map (toDocAndWords' (tdSchema dataSet)) apiDocs

  start <- getCurrentTime
  let ix'  = L.foldl' (\i ((_d,w),did) -> addWordsIx f w did i) ix (zip docs [0..])
  end <- ix' `seq` getCurrentTime

  -- output
  ix' `seq` printStats Nothing ix' (diffUTCTime end start)

-- ----------------------------------------------------------------------------
-- Main

main :: IO ()
main = do
  -- XXX: cycling through without restart leads to inaccurate results
  --mapM_ (\(ds, ix) -> testIndex ds ix) $ zip (repeat dsRandom) (reverse indexes)
  -- test index 0 with dataset random
  -- >> testIndex dsRandom (indexes !! 0)
  testIndex dsRandom (indexes !! 3)
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
  T.sequence . fmap (putStrLn . (++) ">> ") $ titleM
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
