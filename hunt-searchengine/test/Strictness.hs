{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeSynonymInstances      #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ExistentialQuantification #-}

module Main where

import           Hunt.Strict.DocTable
import           Hunt.Strict.Index
import           Hunt.Strict.ContextIndex

import           Test.Framework

main :: IO ()
main = defaultMain $ docTableTests ++ indexTests ++ contextIndexTests

