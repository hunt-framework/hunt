module Fox.Index.InvertedFile.IxFile where

import Fox.Index.InvertedFile.Records as Records
import Fox.Index.InvertedFile.String as String

import qualified Data.Offset as Offset
import qualified Data.Vector.Storable as Vector

data IxFile
  = IxFile { ixOffsets :: Vector.Vector (Offset.OffsetOf
                                          (Records.VocRec String.String))
           }
