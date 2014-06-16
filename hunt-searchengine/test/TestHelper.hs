{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ExistentialQuantification #-}


-- ----------------------------------------------------------------------------
{- |
  Helper and generator for test suites.
-}
-- ----------------------------------------------------------------------------

module TestHelper where

import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random

import           Control.Monad                                   (foldM)

import           Data.Map                                        (Map)
import qualified Data.Map                                        as M
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T

import           Hunt.Common
import qualified Hunt.Common.Positions                       as Pos
import qualified Hunt.Common.Occurrences                     as Occ
import qualified Hunt.Common.DocDesc                         as DD

import qualified Hunt.DocTable                               as Dt
import qualified Hunt.DocTable.HashedDocTable                as HDt
import           Hunt.Utility

import           Data.Time
import           System.Locale


mkInsertList' :: Gen [(Document, Words)]
mkInsertList' = mkDocuments >>= mkInsertList

mkInsertList :: [Document] -> Gen [(Document, Words)]
mkInsertList docs = mapM (\doc -> mkWords >>= \wrds -> return (doc, wrds)) docs

-- --------------------
-- Arbitrary Words

-- using context1 .. context5 as fixed contexts
-- arbitrary context names would not work well in tests
mkWords :: Gen Words
mkWords = mapM addWordsToCx cxs >>= return . M.fromList
  where
  addWordsToCx cx = mkWordList >>= \l -> return (cx,l)
  cxs = map (\i -> T.pack $ "context" ++ (show i)) ([1..5] :: [Int])

mkWordList :: Gen WordList
mkWordList = listOf pair >>= return . M.fromList
  where
  pair = do
    word <- niceText1
    pos  <- listOf arbitrary :: Gen [Int]
    return (word, pos)

instance Arbitrary (HDt.Documents Document) where
  arbitrary = mkDocTable'

mkDocTables :: Gen [(HDt.Documents Document)]
mkDocTables = do
  -- generate list of distinct documents so
  -- that generated doctables are disjunct.
  -- Thats important for some testcases
  docs <- mkDocuments
  mapM mkDocTable $ partitionListByLength 10 docs

mkDocTable' :: Gen (HDt.Documents Document)
mkDocTable' = do
  docs <- mkDocuments
  mkDocTable docs

mkDocTable :: [Document] -> Gen (HDt.Documents Document)
mkDocTable docs = foldM (\dt doc -> Dt.insert doc dt >>= return . snd) Dt.empty docs

instance Arbitrary [Document] where
   arbitrary = mkDocuments

mkDocuments :: Gen [Document]
mkDocuments = do
  numberOfDocuments <- arbitrary :: Gen Int
  mapM mkDocument [1..numberOfDocuments]

instance Arbitrary Document where
   arbitrary = mkDocument'

mkDocument' :: Gen Document
mkDocument' = arbitrary >>= mkDocument

mkDocument :: Int -> Gen Document
mkDocument uri' = do
  d <- mkDescription
  w <- arbitrary
  x <- arbitrary
  return $ Document (T.pack . show $ uri') d (SC w) (SC x)

mkDescription :: Gen Description
mkDescription = do
  txt <- niceText1
  txt2 <- niceText1
  return $ DD.fromList [ ("key1", txt)
                       , ("key2", txt2)
                       ]
-- --------------------
-- Arbitrary Occurrences

instance Arbitrary Occurrences where
  arbitrary = mkOccurrences

mkOccurrences :: Gen Occurrences
mkOccurrences = listOf mkPositions >>= foldM foldOccs Occ.empty
  where
  foldOccs occs ps = do
    docId <- arbitrary :: Gen Int
    return $ Occ.insert' (mkDocId docId) ps occs

mkPositions :: Gen Positions
mkPositions = listOf arbitrary >>= return . Pos.fromList

-- --------------------
-- Arbitrary ApiDocument

apiDocs :: Int -> Int -> IO [ApiDocument]
apiDocs = mkData apiDocGen


mkData :: (Int -> Gen a) -> Int -> Int -> IO [a]
mkData gen minS maxS =
  do rnd0 <- newQCGen --newStdGen
     let rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = System.Random.split rnd
     return [unGen (gen i) r n | ((r,n),i) <- rnds rnd0 `zip` cycle [minS..maxS] `zip` [1..]] -- simple cycle


apiDocGen :: Int -> Gen ApiDocument
apiDocGen n = do
  desc_    <- descriptionGen
  let ix  =  mkIndexData n desc_
  return  $ ApiDocument uri_ ix desc_  1.0 1.0
  where uri_ = T.pack . ("rnd://" ++) . show $ n

niceText1 :: Gen Text
niceText1 = fmap T.pack . listOf1 . elements $ concat [" ", ['0'..'9'], ['A'..'Z'], ['a'..'z']]


descriptionGen :: Gen Description
descriptionGen = do
  tuples <- listOf kvTuples
  return $ DD.fromList tuples
  where
  kvTuples = do
    a <- resize 15 niceText1 -- keys are short
    b <- niceText1
    return (a,b)


mkIndexData :: Int -> Description -> Map Context Content
mkIndexData i d = M.fromList
                $ map (\c -> ("context" `T.append` (T.pack $ show c), prefixx c)) [0..i]
  where
--  index   = T.pack $ show i
  prefixx n = T.intercalate " " . map (T.take n . T.filter (/=' ')) $ values
  values = map (T.pack . show . snd) $ DD.toList d

-- --------------------------------------
-- Other

dateYYYYMMDD :: Gen Text
dateYYYYMMDD = arbitrary >>= \x -> return . T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" (newDate x)
  where
  newDate x = addDays (-x) (fromGregorian 2013 12 31)
