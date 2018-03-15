{-# LANGUAGE DeriveGeneric, RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Fox.Index.MetaFile (
    MfM
  , runMfM
  , writeIndexMetaFile
  ) where

import qualified Fox.Index.Directory as Directory
import qualified Fox.Index.State as Index
import qualified Fox.Schema as Schema
import qualified Fox.Types.Generation as Generation
import qualified Fox.Types.SegmentId as SegmentId
import qualified Fox.Types.SegmentMap as SegmentMap
import qualified Fox.Index.Segment as Segment
import qualified Fox.Types.DocDesc as Document

import qualified Data.Text as Text
import qualified GHC.Generics as Generics
import qualified Data.Binary as Binary

type MfM a = IO a

runMfM :: MfM a -> IO a
runMfM = id

data MetaSegment
  = MetaSegment {
       msegSegmentId  :: !SegmentId.SegmentId
     , msegGeneration :: !Generation.Generation
     , msegNumDocs    :: !Int
     } deriving (Generics.Generic)

data MetaSchema
  = MetaSchema {
        mschFields :: [(MetaFieldName, Schema.FieldType)]
      } deriving (Generics.Generic)

data MetaState
  = MetaState {
       msGeneration    :: !Generation.Generation
     , msSchema        :: !MetaSchema
     , msNextSegmentId :: !SegmentId.SegmentId
     , msSegments      :: [MetaSegment]
     } deriving (Generics.Generic)

type MetaFieldName = Text.Text

-- TODO: orphans, get rid of them
instance Binary.Binary SegmentId.SegmentId
instance Binary.Binary Generation.Generation
instance Binary.Binary Document.FieldType
instance Binary.Binary MetaSegment
instance Binary.Binary MetaState
instance Binary.Binary MetaSchema

writeIndexMetaFile
  :: Directory.MetaDirLayout
  -> Index.State
  -> MfM ()
writeIndexMetaFile Directory.MetaDirLayout{..} state = do

  let
    toMetaSegment :: SegmentId.SegmentId -> Segment.Segment -> MetaSegment
    toMetaSegment segmentId Segment.Segment{..} =
      MetaSegment {
          msegSegmentId  = segmentId
        , msegGeneration = segGeneration
        , msegNumDocs    = segNumDocs
        }

    toMetaFieldName :: Schema.FieldName -> MetaFieldName
    toMetaFieldName = Document.fieldNameToText

    toMetaSchema :: Schema.Schema -> MetaSchema
    toMetaSchema schema =
      MetaSchema {
          mschFields =
              [ (toMetaFieldName fieldName, fieldType)
              | (fieldName, fieldType) <- Schema.toList schema
              ]
        }

    toMetaState :: Index.State -> MetaState
    toMetaState Index.State{..} =
      let
        nextSegId =
          case SegmentMap.findMax ixSegments of
            Just (segId, _) -> SegmentId.nextSegmentId segId
            Nothing         -> SegmentId.genesis

        metaSegments =
          map (uncurry toMetaSegment) (SegmentMap.toList ixSegments)

        metaSchema =
          toMetaSchema ixSchema

      in
        MetaState {
            msGeneration    = ixGeneration
          , msSchema        = metaSchema
          , msNextSegmentId = nextSegId
          , msSegments      = metaSegments
          }

  Binary.encodeFile
    (metaMetaFile (Index.ixGeneration state))
    (toMetaState state)
