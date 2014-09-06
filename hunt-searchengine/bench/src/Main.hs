{-# LANGUAGE OverloadedStrings #-}
module Main where

import           GHC.Stats
import           Control.Lens
import           Control.DeepSeq
import           Data.List.Split.Lens

import           Data.Monoid
import           Data.Aeson
import qualified Data.ByteString.Lazy  as ByteString
import           Data.Int
import qualified Data.Text             as Text
import           Data.Text             (Text)
import           Data.Time

import           System.Mem            (performGC)

import           Hunt.ClientInterface
import           Hunt.Interpreter
import           Hunt.Index.Schema

main :: IO ()
main
  = do
    -- generate test data-sets from files
    dict <- readDict             -- ^ English dictionary
    set1 <- dictSimple dict      -- ^ ApiDocuments contain just single words
    set2 <- dictWithValues dict  -- ^ ApiDocuments contain 100 words each

    ------------------------------------------------------------------------------------
    -- 1. Fulltext benchmarks
    --    Comparison of text 'ContextType's

    -- 1.1)
    -- Benchmark with small-value ApiDocuments.
    -- Each document indexes only one word, thatby 'IndexValue's are small.
    --
    -- Benchmark checks:
    --   - the size and performance of the underlying index data-structure with minimal values
    --
    insertBench "insert11" ctText       set1
    insertBench "insert12" ctTextSimple set1

    deleteBench "delete11" ctText       set1
    deleteBench "delete12" ctTextSimple set1

    -- 1.2)
    -- Benchmark with bigger-values ApiDocuments
    -- Each document indexes about a hundred words, but only 50 unique words.
    -- Therefore: Each of those 50 words contains 2 times in a document
    --
    -- Benchmark checks:
    --   - performance of 'IndexValue' implementation. mergeValues used in inserts, while
    --     diffValues is used in deletes.
    --   - the size and performance of the underlying index data-structure with big values
    --
    insertBench "insert21" ctText       set2
    insertBench "insert22" ctTextSimple set2

    deleteBench "delete21" ctText       set2
    deleteBench "delete22" ctTextSimple set2

    -- force usage of datasets to prevent their garbage collection during a benchmark.
    -- That would propably have an inpact in the benchmarks.
    putStrLn "cleanup..."
    _ <- getAllocatedBytes set1
    _ <- getAllocatedBytes set2
    _ <- getAllocatedBytes dict
    return ()


-- | benchmarks the performance of 10 sequentiell deletes in relation to the size
--   of the index.
deleteBench :: Text -> ContextType -> [ApiDocument] -> IO ()
deleteBench name cxt docs
  = do
    -- format dataset and log memory used by benchmark
    putStrLn $ "run benchmark: " ++ Text.unpack name
    let splits = docs^..chunking 12000 traverse

    -- create hunt instance and run benchmark
    result <- mapM (deleteBench' cxt splits) [1..12]

    -- format result
    let t = zipWith zipTime' [0..] (0:result)

    -- write res to file (js and json)
    writeToFile (Text.unpack name ++ "_performance") t
    return ()

deleteBench' :: ContextType -> [[ApiDocument]] -> Int -> IO Double
deleteBench' cxt docs run
  = do
    putStrLn $ "  interation " ++ show run ++ " of 12"

    -- init index
    let docs' = concat . (take run) $ docs
        uris = take 10 $ drop 1000 $ map adUri docs'

    e     <- initHunt :: IO DefHuntEnv
    _     <- runCmd e $ cmdInsertContext "default" mkSchema { cxType = cxt }
    _     <- runCmd e $ cmdSequence . (map cmdInsertDoc) $ docs'

    -- run delete benchmark on current subset index
    start <- getCurrentTime
    _     <- mapM ((runCmd e) . cmdDeleteDoc) $ uris
    stop  <- getCurrentTime
    let diff = diffUTCTime stop start

    return . realToFrac $ diff


-- | inserts docuemnts in batches and computes allocated memory betweeen each
--   insert. Bytes allocated by benchmark itself are substracted in results
insertBench :: Text -> ContextType -> [ApiDocument] -> IO ()
insertBench name cxt docs
  = do
    -- format dataset and log memory used by benchmark
    putStrLn $ "run benchmark: " ++ Text.unpack name
    let splits = docs^..chunking 12000 traverse
    overhead <- getAllocatedBytes splits

    -- create hunt instance and run benchmark
    e <- initHunt :: IO DefHuntEnv
    _ <- runCmd e $ cmdInsertContext "default" mkSchema { cxType = cxt }
    zero <- getAllocatedBytes ()
    memAndTime <- mapM (insertBench' e) splits

    -- format result
    let result = ((zero,0):memAndTime)
        m = zipWith (zipMem overhead) [0..] result
        t = zipWith zipTime [0..] result

    -- write res to file (js and json)
    writeToFile (Text.unpack name ++ "_performance") t
    writeToFile (Text.unpack name ++ "_memory") m
    return ()

insertBench' :: DefHuntEnv -> [ApiDocument] -> IO (Int64, Double)
insertBench' e docs
  = do
    start <- getCurrentTime
    res   <- runCmd e $ cmdSequence $ map cmdInsertDoc docs
    stop  <- getCurrentTime
    let diff = diffUTCTime stop start
    bytes <- getAllocatedBytes res
    return (bytes, realToFrac diff)

-- ------------------------------------------------------
-- Create Data-Set helper

-- | Read english dictionary from file
readDict :: IO [(Int, Text)]
readDict
  = do
    content <- readFile "./data/en_US.dict"
    let ws  = map Text.pack $ lines content
    return $!! mkIndexList ws 1 []
  where
    mkIndexList []     _ a = a
    mkIndexList (x:xs) i a = mkIndexList xs (i+1) ((i,x):a)

-- | Creates data-set from dictionary:
--   Every word represents a document. Thatby the values to
--   put into the index are very small (one word)
dictSimple :: [(Int,Text)] -> IO [ApiDocument]
dictSimple dict
  = do
    let res = map mkApiDocument dict
    _ <- getAllocatedBytes res
    return res
  where
    mkApiDocument (i,w) = addToIndex "default" w $
                          mkApiDoc . Text.pack . show $ i

-- | Creates data-set from dictionary:
--   Every word represents a document. The content to put into
--   the index is generated from the next 50 words of the
--   dictionary. Thatby it is garanteed that most words are
--   occurring in at least 50 documents.
--   This is important to test the 'IndexValue's merge operation
dictWithValues :: [(Int, Text)] -> IO [ApiDocument]
dictWithValues dict
  = do
    let res = mapDict dict []
    _ <- getAllocatedBytes res
    return res
  where
    mapDict []         r = r
    mapDict xss@(x:xs) r = mapDict xs ((mkApiDocument x xss):r)

    mkApiDocument (i, _) rest
      = addToIndex "default" (mkContent rest) $
        mkApiDoc . Text.pack . show $ i

    mkContent r = mkWords r `mappend` mkWords r
    mkWords = (Text.intercalate " ") . (map snd) . (take 50)



-- | perform GC, then return bytesAllocated by process
getAllocatedBytes :: NFData x => x -> IO Int64
getAllocatedBytes x = getStats x >>= return . currentBytesUsed

-- | peform GC, then get GCStats
getStats :: NFData x => x -> IO GCStats
getStats x = do
  x `deepseq` performGC
  getGCStats

writeToFile :: ToJSON r => String -> r -> IO ()
writeToFile n res
  = do
    writeFile (n ++ ".js") $ concat [ "var ", n, " = " ]
    ByteString.appendFile (n ++ ".js") (encode res)
    ByteString.writeFile (n ++ ".json") (encode res)

zipMem :: Int64 -> Int -> (Int64, x) -> (Int, Int64)
zipMem overhead i (b,_) = (i*12000, (b - overhead) `div` 1024)

zipTime :: Int -> (x, Double) -> (Int, Double)
zipTime i (_,t) = (i*12000, t)

zipTime' :: Int -> Double -> (Int, Double)
zipTime' i t = (i*12000, t)

