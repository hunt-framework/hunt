{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances  #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Common3 where

import           Control.Applicative      ((<$>))
import           Control.Arrow            (first, second)
import           GHC.Exts                 (Constraint)
import qualified Data.StringMap           as SM
import           AssocTypes2              (Index(..), ContextWordMap, Compression(..), ByteString)        
-- ----------------------------------------
-- type aliases

type DocId      = Int
type Context    = String
type Word       = String
type Document   = String
type Result     = Maybe Occs
type Occs       = [(DocId, Positions)]
type WordList   = [(Word, Positions)]
type Positions  = [Int]

--------------------------------------------
-- StringMap as index ... simple
instance Index SM.StringMap where
    type IKey SM.StringMap v = Word
    type IVal SM.StringMap v = v
    type IToL SM.StringMap v = [(Word,v)]
    
    insert = SM.insert
    delete = SM.delete
    empty  = SM.empty
    fromList = SM.fromList
    toList = SM.toList
    search = SM.prefixFindWithKey
--------------------------------------------
-- how to integrate enum for different search operations?
-- 
-- another "type" declaration? like:
--
-- type IOp i v :: *
-- search :: (ICon i v) => IOp i v -> ...
-- type IOp SM.StringMap v = TextIndex
--
-- or with another composed key, like this:

newtype PTText v = PTText (SM.StringMap v)
data TextIndex = Case | NoCase -- | .. | .. | ..

instance Index PTText where
   type IKey PTText v = (TextIndex, Word)
   type IVal PTText v = v
   type IToL PTText v = [(Word, v)]

   insert (_,k) v    (PTText i) = PTText $ SM.insert k v i
   delete (_,k)      (PTText i) = PTText $ SM.delete k i
   empty                        = PTText $ SM.empty
   fromList l                   = PTText $ SM.fromList l
   toList            (PTText i) = SM.toList i
   search (Case,k)   (PTText i) = SM.prefixFindWithKey k i
   search (NoCase,k) (PTText i) = SM.prefixFindWithKeyBF k i
  
occs :: Occs
occs = [(1, [1,2,6,8,44,77,32])]

xs1, xs2 :: [(String, Occs)]
xs1 = [("word1", [(1, [1,5,7])])]
xs2 = [("word2", [(4, [3,5,7])])]

type Inverted = ContextWordMap SM.StringMap Occs
cx1, cx2, cx3, cx4, cx5, cx6, cx7, cx8 :: Inverted

cx1 = fromList [("A",xs1),("B",xs2)]
cx2 = insert (Just "C", Just "111") occs cx1
cx3 = insert (Just "A", Just "111") occs cx2
cx4 = insert (Nothing, Just "ddd") occs cx3
cx5 = delete (Nothing, Just "ddd") cx4
cx6 = delete (Just "A", Just "abc") cx5
cx7 = delete (Just "A", Nothing) cx6
cx8 = delete (Nothing, Nothing) cx7

