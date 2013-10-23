module Main where

import Holumbus.Interpreter.Interpreter

main1 :: Command -> IO ()
main1 c
    = do env0 <- initEnv emptyIndexer emptyOptions
         let eval = runCmd env0
         eval c >>= print
         return ()

-- ----------------------------------------------------------------------------

c1 = NOOP
c2 = Search ("abc")
c3 = LoadIx  "ix1"
c4 = StoreIx "ix2"
c5 = Sequence [c1,c2,c3,c4]
