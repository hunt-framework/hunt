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

import           Test.Framework
import           Test.Framework.Providers.QuickCheck2
import           System.Random
import           Test.QuickCheck
import           Test.QuickCheck.Gen
import           Test.QuickCheck.Random

import           Control.Monad                                   (foldM)
import           Control.Arrow                                   (second)

import           Data.Maybe                                      (fromMaybe)
import           Data.Map                                        (Map)
import qualified Data.Map                                        as M
import qualified Data.Set                                        as S
import           Data.Text                                       (Text)
import qualified Data.Text                                       as T
import           Data.Aeson                                      (fromJSON)
import qualified Data.HashMap.Strict                             as HM

import           Hunt.Common
import qualified Hunt.Common.Positions                       as Pos
import qualified Hunt.Common.Occurrences                     as Occ
import qualified Hunt.Common.DocIdMap                        as DM
import qualified Hunt.Common.DocDesc                         as DD
import qualified Hunt.Common.DocIdSet                        as IS

import           Data.Time
import           System.Locale


mkInsertList :: Gen [(Document, Words)]
mkInsertList = listOf entries
  where
  entries = do
    doc <- mkDocument
    wrds <- mkWords
    return (doc, wrds)

-- --------------------
-- Arbitrary Words

-- using context1 .. context5 as fixed contexts
-- arbitrary context names would not work well in tests
mkWords :: Gen Words
mkWords = mapM addWordsToCx cxs >>= return . M.fromList
  where
  addWordsToCx cx = mkWordList >>= \l -> return (cx,l)
  cxs = map (\i -> T.pack $ "context" ++ (show i)) [1..5]

mkWordList :: Gen WordList
mkWordList = listOf pair >>= return . M.fromList
  where
  pair = do
    word <- niceText1
    pos  <- listOf arbitrary
    return (word, pos)

-- Arbitrary Documents

instance Arbitrary Document where
  arbitrary = mkDocument

mkDocuments :: Gen [Document]
mkDocuments = listOf arbitrary

mkDocument :: Gen Document
mkDocument = do
  u <- niceText1
  d <- mkDescription
  w <- arbitrary
  x <- arbitrary
  return $ Document u d (SC w) (SC x)


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
     let rnds rnd = rnd1 : rnds rnd2 where (rnd1,rnd2) = split rnd
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
  prefixx n = T.intercalate " " . map (T.take n . T.filter (/=' ')) $ map (fromMaybe "") values
  values = undefined --map (fromJSON . snd) $ DD.toList d

toList (DD.DocDesc m) = (map (second fromJSON) $ HM.toList m)

-- --------------------------------------
-- Other

dateYYYYMMDD :: Gen Text
dateYYYYMMDD = arbitrary >>= \x -> return . T.pack $ formatTime defaultTimeLocale "%Y-%m-%d" (newDate x)
  where
  newDate x = addDays (-x) (fromGregorian 2013 12 31)
