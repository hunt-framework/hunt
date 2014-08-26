{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import           Hunt.AnalyzerTests
import           Hunt.IndexTests
import           Hunt.InterpreterTests
import           Hunt.QueryParserTests
--import           Hunt.RankingTests

import           Test.Framework

main :: IO ()
main = defaultMain
       $ analyzerTests
       ++ contextTypeTests
       ++ interpreterTests
       ++ queryParserTests
--     XXX refactor ranking tests to be conform with new ranking
--       ++ rankingTests
