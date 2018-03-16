module Fox.Index.InvertedFile.TermIndex where

import Fox.Index.InvertedFile.Records as Records
import Fox.Index.InvertedFile.String as String

import qualified Data.Offset as Offset
import qualified Data.Vector.Storable as Vector

newtype TermIndex
  = TermIndex { ixOffsets :: Vector.Vector Records.IxRec
              }
