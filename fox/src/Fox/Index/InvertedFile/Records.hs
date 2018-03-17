{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Fox.Index.InvertedFile.Records where

import qualified Data.Coerce as Coerce
import qualified Data.Word as Word
import qualified Fox.IO.Read as Read
import qualified Fox.IO.Write as Write
import qualified Fox.Schema as Schema
import qualified Fox.Types.Document as Document
import qualified Fox.Types.Occurrences as Occurrences
import qualified Fox.Types.Positions as Positions
import qualified Fox.Types.Token as Token

import qualified Data.Count as Count
import qualified Data.Offset as Offset
import qualified Foreign.Storable as Storable
import qualified Foreign.Ptr as Ptr

-- | Convenience type for serialiasing occurrences for
-- the occurrence file.
data OccRec
  = OccRec { owDocId     :: !Document.DocId
           , owPosCount  :: !(Count.CountOf Positions.Position)
           , owPosOffset :: !(Offset.OffsetOf Positions.Position)
           }

occurrenceWrite :: Write.Write OccRec
occurrenceWrite =
  (owDocId     Write.>$< docIdWrite) <>
  (owPosCount  Write.>$< countOfWrite) <>
  (owPosOffset Write.>$< offsetOfWrite)

occurrenceRead :: Read.Read OccRec
occurrenceRead =
  OccRec <$> docIdRead
         <*> countOfRead
         <*> offsetOfRead

-- | 'VocWrite' represents a row in the vocabulary file.
-- 'VocRec' is abstract in the term field.
data VocRec term
  = VocRec { vwLengthPrefix :: !(Count.CountOf Word.Word16)
           , vwTerm         :: !term
           , vwField        :: !Schema.FieldOrd
           , vwOccCount     :: !(Count.CountOf Occurrences.Occurrences)
           , vwOccOffset    :: !(Offset.OffsetOf Occurrences.Occurrences)
           } deriving (Show)

vocWrite :: Write.Write (VocRec Token.Term)
vocWrite =
  (vwLengthPrefix Write.>$< countOfWrite) <>
  (vwTerm         Write.>$< termWrite) <>
  (vwField        Write.>$< fieldOrdWrite) <>
  (vwOccCount     Write.>$< countOfWrite) <>
  (vwOccOffset    Write.>$< offsetOfWrite)

vocRead :: Read.Read (VocRec Read.UTF16)
vocRead =
  VocRec <$> countOfRead
         <*> Read.utf16
         <*> fieldOrdRead
         <*> countOfRead
         <*> offsetOfRead

-- | 'IxWrite' represents a row in the vocabulary lookup file
data IxRec
  = IxRec { ixVocOffset          :: !(Offset.OffsetOf (VocRec Read.UTF16))
          , ixPrecedingTermCount :: !(Count.CountOf (VocRec Read.UTF16))
          } deriving (Eq, Ord, Show
                     )

instance Storable.Storable IxRec where
  sizeOf _ =
    Storable.sizeOf (ixVocOffset undefined) +
    Storable.sizeOf (ixPrecedingTermCount undefined)
  alignment _ =
    Storable.alignment (ixVocOffset undefined)
  peek op =
    let
      sizeVocOffset =
        Storable.sizeOf (ixVocOffset undefined)
    in
      IxRec <$> Storable.peek (Ptr.castPtr op)
            <*> Storable.peek (Ptr.castPtr op `Ptr.plusPtr` sizeVocOffset)
  poke op IxRec{..} = do
    let
      sizeVocOffset =
        Storable.sizeOf ixVocOffset
    Storable.poke (Ptr.castPtr op) ixVocOffset
    Storable.poke (Ptr.castPtr op `Ptr.plusPtr` sizeVocOffset) ixPrecedingTermCount

ixWrite :: Write.Write IxRec
ixWrite =
  (ixVocOffset          Write.>$< offsetOfWrite) <>
  (ixPrecedingTermCount Write.>$< countOfWrite)

ixRead :: Read.Read IxRec
ixRead =
  IxRec <$> offsetOfRead
        <*> countOfRead

positionWrite :: Write.Write Positions.Position
positionWrite = Write.varint

fieldOrdWrite :: Write.Write Schema.FieldOrd
fieldOrdWrite = Write.varint

fieldOrdRead :: Read.Read Schema.FieldOrd
fieldOrdRead = Read.varint

termWrite :: Write.Write Token.Term
termWrite = Write.text

-- | Various 'Write's for the types we want to serialize
-- down the road.
offsetOfWrite :: Write.Write (Offset.OffsetOf a)
offsetOfWrite =
  Coerce.coerce Write.>$< Write.varint

offsetOfRead :: Read.Read (Offset.OffsetOf a)
offsetOfRead =
  Coerce.coerce `fmap` Read.varint

countOfWrite :: Write.Write (Count.CountOf a)
countOfWrite =
  Coerce.coerce Write.>$< Write.varint

countOfRead :: Read.Read (Count.CountOf a)
countOfRead =
  Coerce.coerce `fmap` Read.varint

docIdWrite :: Write.Write Document.DocId
docIdWrite =
  Document.unDocId Write.>$< Write.varint

docIdRead :: Read.Read Document.DocId
docIdRead =
  Coerce.coerce `fmap` Read.varint
