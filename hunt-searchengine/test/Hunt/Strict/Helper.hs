module Hunt.Strict.Helper where

import           Test.QuickCheck
import           Test.QuickCheck.Monadic                         (PropertyM,
                                                                  monitor,
                                                                  run)
import           GHC.AssertNF
import           GHC.HeapView
import qualified System.Mem

heapGraph :: Int -> a -> IO String
heapGraph d x = do
  let box = asBox x
  graph <- buildHeapGraph d () box
  return $ ppHeapGraph graph

isNFWithGraph :: Int -> a -> IO (Bool, String)
isNFWithGraph d x = do
  b <- isNF $! x
  -- XXX: does gc need a delay?
  System.Mem.performGC
  g <- heapGraph d x
  return (b,g)

-- depth is a constant
assertNF' :: a -> PropertyM IO ()
assertNF' = assertNF'' 5

assertNF'' :: Int -> a -> PropertyM IO ()
assertNF'' d x = do
  (b,g) <- run $ isNFWithGraph d x
  monitor $ const $ counterexample  g b


