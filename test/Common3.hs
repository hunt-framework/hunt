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
import qualified Holumbus.Data.PrefixTree as PT
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
-- prefixtree as index ... simple
instance Index PT.PrefixTree where
    type IKey PT.PrefixTree v = Word
    type IVal PT.PrefixTree v = v
    type IToL PT.PrefixTree v = [(Word,v)]
    
    insert = PT.insert 
    delete = PT.delete
    empty  = PT.empty
    fromList = PT.fromList
    toList = PT.toList
    search = PT.prefixFindWithKey
--------------------------------------------
-- how to integrate enum for different search operations?
-- 
-- another "type" declaration? like:
--
-- type IOp i v :: *
-- search :: (ICon i v) => IOp i v -> ...
-- type IOp PT.PrefixTree v = TextIndex
--
-- or with another composed key, like this:

newtype PTText v = PTText (PT.PrefixTree v)
data TextIndex = Case | NoCase -- | .. | .. | ..

instance Index PTText where
   type IKey PTText v = (TextIndex, Word)
   type IVal PTText v = v
   type IToL PTText v = [(Word, v)]

   insert (_,k) v    (PTText i) = PTText $ PT.insert k v i
   delete (_,k)      (PTText i) = PTText $ PT.delete k i
   empty                        = PTText $ PT.empty
   fromList l                   = PTText $ PT.fromList l
   toList            (PTText i) = PT.toList i  
   search (Case,k)   (PTText i) = PT.prefixFindWithKey k i
   search (NoCase,k) (PTText i) = PT.prefixFindWithKeyBF k i
  
occs :: Occs
occs = [(1, [1,2,6,8,44,77,32])]

xs1, xs2 :: [(String, Occs)]
xs1 = [("word1", [(1, [1,5,7])])]
xs2 = [("word2", [(4, [3,5,7])])]

type Inverted = ContextWordMap PT.PrefixTree Occs
cx1, cx2, cx3, cx4, cx5, cx6, cx7, cx8 :: Inverted

cx1 = fromList [("A",xs1),("B",xs2)]
cx2 = insert (Just "C", Just "111") occs cx1
cx3 = insert (Just "A", Just "111") occs cx2
cx4 = insert (Nothing, Just "ddd") occs cx3
cx5 = delete (Nothing, Just "ddd") cx4
cx6 = delete (Just "A", Just "abc") cx5
cx7 = delete (Just "A", Nothing) cx6
cx8 = delete (Nothing, Nothing) cx7

