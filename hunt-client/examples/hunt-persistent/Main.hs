{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Trans.Class (lift)

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import           Data.Conduit (($$), ($=))
import qualified Data.Map.Lazy as M
import           Data.String.Conversions (cs)
import           Data.Aeson.Encode.Pretty (encodePretty)

import qualified Hunt.Conduit as HC
import qualified Hunt.Server.Client as HC
import qualified Hunt.ClientInterface as H

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]

personToApiDoc :: Entity Person -> H.ApiDocument
personToApiDoc ebp = H.listToApiDoc (cs $ show key) l l
    where
    key = unKey $ entityKey ebp
    (Person n a) = entityVal ebp
    l = [("name", cs n), ("age", cs $ show $ a)]

main :: IO ()
main = runSqlite ":memory:" $ do
     (flip HC.withHuntServer) "http://localhost:3000" $ do
        lift $ do
            runMigration migrateAll
            insert $ Person "John Doe" 35
            insert $ Person "Jane Doe" 34

        let source = selectSource [] []
            inserts = HC.makeInserts (personToApiDoc)
            pipe = source $= inserts $$ HC.mergeInserts

        insertDocCmds <- lift $ pipe

        let insertCtxCmds = H.createContextCommands $ H.insertCmdsToDocuments insertDocCmds
            cmds = H.cmdSequence [insertCtxCmds, insertDocCmds]
        res <- HC.eval cmds
        liftIO $ print res
