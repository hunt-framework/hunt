module Fox.Index.InvertedFile.IxFile where

import qualified Data.Offset as Offset
import qualified Data.Vector.Storable as Vector

type VocRec
  = String

data IxFile
  = IxFile { ixOffsets :: Vector.Vector (Offset.OffsetOf VocRec)
           }
