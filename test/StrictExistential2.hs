{-# LANGUAGE ExistentialQuantification #-}

import GHC.AssertNF
import Control.DeepSeq
import Data.Map.Strict

-- 1) simple container

data Obj a = Obj a

-- using a smart constructor here to ensure arbitrary values are strict
mkObj :: a -> Obj a
mkObj a = Obj $! a

-- using a special String constructor to ensure Strings are always 
-- fully evaluated in this example
mkString :: String -> String
mkString x = force x

xs :: Map Int (Obj String)
xs = fromList [ (1, mkObj . mkString $ "abc")
              , (2, mkObj . mkString $ "def")
              , (3, mkObj . mkString $ "hij")
              ]

-- 2) container using existential quantification

data Obj2 = forall a. (Show a) => Obj2 a

-- using the smart constructor here has no effect on strictness
mkObj2 :: Show a => a -> Obj2
mkObj2 a = Obj2 $! a

xs2 :: Map Int Obj2
xs2 = fromList [ (1, mkObj2 1)
               , (2, mkObj2 . mkString $ "test")
               , (3, mkObj2 'c')
               ]

-- 3) container using existential quantification and deepseq

data Obj3 = forall a. (NFData a, Show a) => Obj3 !a

instance NFData Obj3 where
  -- using default implementation

-- even forcing full evaluation with deepseq has no effect on strictness
mkObj3 :: (NFData a, Show a) => a -> Obj3
mkObj3 a = Obj3 $!! a

xs3 :: Map Int Obj3
xs3 = fromList [ (1, mkObj3 (1::Int))
               , (2, mkObj3 . mkString $ "abc")
               , (3, mkObj3 ('c'::Char))
               ]

-- strictness tests
main :: IO ()
main = do
  putStr "test: simple container: "
  (isNF $! xs) >>= putStrLn . show
  assertNF $! xs
  putStr "test: heterogeneous container: "
  (isNF $! xs2) >>= putStrLn . show
  assertNF $! xs2
  putStr "test: heterogeneous container with NFData: " 
  (isNF $!! xs3) >>= putStrLn . show
  assertNF $!! xs3 
  return ()


