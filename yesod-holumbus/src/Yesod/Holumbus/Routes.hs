module Yesod.Hunt.Routes where

import           Data.Text

import           Hunt.Common
import qualified Hunt.Interpreter.Interpreter as Hol

import           Hunt.Index.Index
import           Hunt.DocTable.DocTable
import           Hunt.Index.InvertedIndex
import           Hunt.DocTable.HashedDocuments

import Yesod

-- | wrapper type for index environment
data Holumbus = Holumbus { getHolumbus :: Hol.Env InvertedIndex (Documents Document) }

-- | helper for easy initiation
emptyIndex :: IO Holumbus
emptyIndex = do
  env <- Hol.initEnv Hol.emptyIndexer Hol.emptyOptions
  return $ Holumbus env

-- | class that has to be implemented for yesod master application
--   TODO: acutally use that type declarations!
class ( Yesod master
      , Index (HolIndexImpl master)
      , DocTable (HolDocTableImpl master)
      ) => YesodHolumbus master where

    type HolIndexImpl master :: * -> *
    type HolIndexImpl master = InvertedIndex

    type HolDocTableImpl master :: *
    type HolDocTableImpl master = Documents Document


-- | TemplateHaskell magic: create Types for routes with
--   that small QQ-Dsl then generate Yesod Dispatch
mkYesodSubData "Holumbus" [parseRoutes|
/search/#String            HolSearch         GET
/search/#String/#Int/#Int  HolPagedSearch    GET
/completion/#String        HolCompletion     GET
/insert                    HolInsert         POST
/delete                    HolDelete         POST
|]

