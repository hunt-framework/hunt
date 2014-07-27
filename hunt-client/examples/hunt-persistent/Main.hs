{-# LANGUAGE EmptyDataDecls    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE FlexibleInstances #-}

import           Control.Applicative     ((<$>))
import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Trans.Class (lift)

import           Database.Persist
import           Database.Persist.Sqlite
import           Database.Persist.TH

import qualified Data.Map.Lazy as M
import           Data.String.Conversions (cs)
import           Data.Aeson.Encode.Pretty (encodePretty)

import qualified Hunt.Server.Client as HC
import           Hunt.ClientInterface (Huntable(..))
import qualified Hunt.ClientInterface as H

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Person
    name String
    age Int
    deriving Show
|]

instance Huntable (Entity Person) where
    huntURI
      = cs . show . unKey . entityKey

    huntIndexMap (Entity _ (Person n a))
      = M.fromList [("name", cs n), ("age", cs . show $ a)]

-- like persistent insert, but returns hunt insert command instead
-- of just the persistent id (as persistent insert does)
insertH e
  = do
    id <- insert e
    return . H.cmdInsertDoc . toApiDocument $ Entity id e

main :: IO ()
main = runSqlite ":memory:" $ do
     (flip HC.withHuntServer) "http://localhost:3000" $ do

        let persons = [ Person "John Doe" 35
                      , Person "Jane Doe" 25
                      ]

        huntInsertCmds <- lift $ do
            runMigration migrateAll
            mapM insertH persons

        let insertSequence = H.cmdSequence huntInsertCmds
            insertCtxCmds  = H.createContextCommands . H.insertCmdsToDocuments $ insertSequence

        res <- HC.eval $ H.cmdSequence [insertCtxCmds, insertSequence]
        liftIO $ print res
