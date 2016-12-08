{-# LANGUAGE ExistentialQuantification #-}
module Fox.Types.Document where

import           Fox.Types.DocDesc (Field, FieldValue)

data Document =
  Document { docWeight :: !Float
           , docFields :: [DocField]
           }

type Analyzer a = ()

data DocField =
  forall a. DocField { fieldName    :: !Field
                     , fieldValues  :: [a]
                     , fieldWeight  :: !Float
                     }

emptyDocument :: Document
emptyDocument = Document 0.0 []
