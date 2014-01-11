{-# LANGUAGE ExistentialQuantification #-}

import GHC.AssertNF
import Control.DeepSeq
import Data.Map.Strict

-- data container using existential quantification
data Obj = forall a. (Show a) => Obj a

mkObj :: Show a => a -> Obj
mkObj a = Obj $! a

xs :: Map Int Obj
xs = fromList [(1, mkObj 1), (2,mkObj 'r'), (3,mkObj 'c')]

doShow :: [Obj] -> String
doShow [] = ""
doShow ((Obj x):xs) = show x ++ doShow xs

-- simple container
data Obj2 a = Obj2 a

mkObj2 :: a -> Obj2 a
mkObj2 a = Obj2 $! a

xs2 :: Map Int (Obj2 Char)
xs2 = fromList [(1, mkObj2 'r'), (2, mkObj2 'd'), (3,mkObj2 'k')]

-- test 
main :: IO ()
main = do
  putStrLn "test simple:"
  assertNF $! xs2
  putStrLn "test existential:"
  assertNF $! xs
  return ()


