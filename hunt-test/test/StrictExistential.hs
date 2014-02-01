{-# LANGUAGE ExistentialQuantification #-}

import           GHC.AssertNF
--import           Control.DeepSeq
import           Data.Map.Strict

-- data container using existential quantification
data Obj = forall a. (Show a) => Obj a

instance Show Obj where
  showsPrec p (Obj x) = showParen (p > 10) $ showString "Obj " . shows x

mkObj :: Show a => a -> Obj
mkObj a = Obj $! a

objs :: Map Int Obj
objs = fromList [(1, mkObj 1), (2, mkObj 'r'), (3, mkObj 'c')]

-- simple container
data Obj2 a = Obj2 a
  deriving Show

mkObj2 :: a -> Obj2 a
mkObj2 a = Obj2 $! a

objs2 :: Map Int (Obj2 Char)
objs2 = fromList [(1, mkObj2 'r'), (2, mkObj2 'd'), (3, mkObj2 'k')]

-- test
main :: IO ()
main = do
  putStrLn $ "test simple: " -- ++ show objs2
  assertNF $! objs2
  putStrLn $ "test existential: " -- ++ show objs
  assertNF $! objs
  return ()
