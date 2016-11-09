{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RecordWildCards       #-}
module Hunt.SegmentIndex.Store where

import           GHC.Generics

import           Hunt.Common.BasicTypes
import           Hunt.Index.Schema
import           Hunt.SegmentIndex.Store.DirLayout
import           Hunt.SegmentIndex.Types
import           Hunt.SegmentIndex.Types.Generation
import           Hunt.SegmentIndex.Types.Index
import           Hunt.SegmentIndex.Types.SegmentId
import           Hunt.SegmentIndex.Types.SegmentMap
import qualified Hunt.SegmentIndex.Types.SegmentMap as SegmentMap

import           Control.Exception
import           Control.Monad.Except
import           Data.Binary                        (Binary)
import qualified Data.Binary                        as Binary
import           Data.Map                           (Map)
import qualified Data.Map.Strict                    as Map
import           Data.Store
import           Data.Text                          (Text)
import           Data.Traversable
import           System.FilePath                    ((</>))

-- | A simplified representation for 'Segment' which is used
-- to store 'Segment's on disk.
data SegmentInfo =
  SegmentInfo {
     segiNumDocs     :: !Int
   , segiDelGen      :: !Generation
   , segiContextInfo :: !(Map Context Int)
   } deriving (Generic)

instance Binary SegmentInfo

data SegmentInfos =
  SegmentInfos {
      sisSegmentIdGen :: !SegmentId
    , sisSchema       :: !Schema
    , sisSegmentInfos :: !(SegmentMap SegmentInfo)
    } deriving (Generic)

instance Binary SegmentInfos

segmentToSegmentInfo :: Segment -> SegmentInfo
segmentToSegmentInfo Segment{..} = SegmentInfo {
    segiNumDocs     = segNumDocs
  , segiDelGen      = segDelGen
  , segiContextInfo = Map.map indexReprNumTerms segTermIndex
  }

segmentIndexToSegmentInfos :: SegmentIndex -> SegmentInfos
segmentIndexToSegmentInfos SegmentIndex{..} =
  let
    segIdCount :: SegmentId
    segIdCount = case SegmentMap.findMax siSegments of
      Just (maxSegId, _) -> succ maxSegId
      _                  -> segmentZero
  in SegmentInfos {
      sisSegmentIdGen = segIdCount
    , sisSchema       = siSchema
    , sisSegmentInfos = SegmentMap.map segmentToSegmentInfo siSegments
    }

data IndexLoadError = ErrContextTypeNotFound
                    | ErrNormalizerNotFound
                    | ErrDecodingFailed
                    deriving (Show)

instance Exception IndexLoadError

segmentInfosToSegmentIndex :: FilePath
                           -> Generation
                           -> (Text -> IO (Maybe ContextType))
                           -> (Text -> IO (Maybe CNormalizer))
                           -> SegmentInfos
                           -> ExceptT IndexLoadError IO (GenSegmentIndex SegmentInfo)
segmentInfosToSegmentIndex indexDirectory
                           generation
                           askContextType
                           askNormalizer
                           SegmentInfos{..} =
  let
    withError :: (MonadIO m, MonadError e m)
              => e
              -> (x -> IO (Maybe b))
              -> x -> m b
    withError e f = \x -> do
      mx <- liftIO $ f x
      case mx of
        Just x  ->  return x
        Nothing -> throwError e

    askContextType' =
      withError ErrContextTypeNotFound askContextType

    askNormalizer' = do
      withError ErrNormalizerNotFound askNormalizer

  in do
    schema <- for sisSchema $ \cschema -> do
      cxtype <- askContextType'
                . ctName
                . cxType
                $ cschema
      cnorms <- for (cxNormalizer cschema) (askNormalizer' . cnName)
      return cschema { cxType       = cxtype
                     , cxNormalizer = cnorms
                     }

    segIdGen <- liftIO $ newSegIdGen' sisSegmentIdGen
    return SegmentIndex {
        siGeneration = generation
      , siIndexDir   = indexDirectory
      , siSegIdGen   = segIdGen
      , siSchema     = schema
      , siSegments   = sisSegmentInfos
      , siSegRefs    = SegmentMap.empty
      }

readSegmentInfos :: FilePath
                 -> Generation
                 -> ExceptT IndexLoadError IO SegmentInfos
readSegmentInfos indexDirectory generation = do
  msi <- liftIO $ Binary.decodeFileOrFail
         (indexDirectory </> segmentInfosFile generation)
  case msi of
    Right si -> return si
    Left _   -> throwError ErrDecodingFailed

storeSegmentInfos :: FilePath
                  -> Generation
                  -> SegmentInfos
                  -> IO ()
storeSegmentInfos indexDirectory generation segmentInfos = do
  Binary.encodeFile
    (indexDirectory </> segmentInfosFile generation)
    segmentInfos
