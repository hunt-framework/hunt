module Hunt.SegmentIndex.Open (
    openOrNewSegmentIndex
  , AccessMode (..)
  , AtRevision (..)
  , IndexOpenError (..)
  ) where

import           Hunt.SegmentIndex.Types
import           Hunt.SegmentIndex.Types.Generation
import           Hunt.SegmentIndex.Types.SegmentId
import qualified Hunt.SegmentIndex.Types.SegmentMap as SegmentMap

import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.Except
import qualified Data.List                          as List
import qualified Data.Map                           as Map
import           Data.Ord
import qualified System.Directory                   as Directory
import           Text.Read

data IndexOpenError = ErrRevisionNotFound
                    | ErrInvalidIndexDirectory
                    deriving (Eq, Show)

instance Exception IndexOpenError

data AccessMode = AccessReadOnly
                | AccessReadWrite

data AtRevision = RevHead
                | RevSpec Generation

openOrNewSegmentIndex :: FilePath
                      -> AccessMode
                      -> AtRevision
                      -> IO (Either IndexOpenError SegIxRef)
openOrNewSegmentIndex indexDirectory accessMode atRevision = runExceptT $ do

  indexDirIsFile <- liftIO $ Directory.doesFileExist indexDirectory
  when indexDirIsFile $ do
    throwError ErrInvalidIndexDirectory

  indexDirExists <- liftIO $ Directory.doesDirectoryExist indexDirectory
  unless indexDirExists $ do
    liftIO $ Directory.createDirectoryIfMissing True indexDirectory

  ixfiles <- liftIO $ Directory.listDirectory indexDirectory

  let
    wantsRevSpec :: Bool
    wantsRevSpec = case atRevision of
                     RevSpec{} -> True
                     _         -> False

    -- since opening a 'SegmentIndex' at a specific
    -- revision is prone to overwrite alread present
    -- 'Segment's we switch to read-only access in
    -- case of a specific revision to load.
    realAccessMode :: AccessMode
    realAccessMode | wantsRevSpec = AccessReadOnly
                   | otherwise    = accessMode

    -- take the files in the index directory and
    -- filter the 'gen_N' files. Sorted descendingly.
    revisions :: [Generation]
    revisions = List.sortBy (comparing Down)
                $ concatMap f ixfiles
      where
        f ('g':'e':'n':'_':xs)
          | Just gen <- readMaybe xs = [gen]
        f _                          = []

  case revisions of
    -- there were no generations saved in the directory
    -- just create a new 'SegmentIndex'
    []    -> liftIO $ newSegmentIndex indexDirectory

    -- we want a specific generation and this is the most
    -- recent. Open the index with any 'AccessMode'.
    (gen:_)
      | RevSpec wanted <- atRevision
      , gen == wanted -> openSegmentIndex indexDirectory accessMode wanted

    -- we want a specific generation and its somewhere between
    -- many others. We open the 'SegmentIndex' with 'AccessReadOnly'
    -- to prevent overwrite of existing 'Segment's.
    gens
      | RevSpec wanted <- atRevision
      , wanted `List.elem` gens ->
          openSegmentIndex indexDirectory AccessReadOnly wanted

    -- we want the HEAD generation we can access it here with any
    -- 'AccessMode'.
      | otherwise ->
          openSegmentIndex indexDirectory accessMode (head gens)

openSegmentIndex :: FilePath
                 -> AccessMode
                 -> Generation
                 -> ExceptT IndexOpenError IO SegIxRef
openSegmentIndex indexDirectory accessMode generation = do
  undefined

-- | Create a new 'SegmentIndex' in the given directory.
newSegmentIndex :: FilePath -> IO SegIxRef
newSegmentIndex indexDirectory = do
  segIdGen <- newSegIdGen
  newMVar $! SegmentIndex {
      siGeneration = generationZero
    , siIndexDir   = indexDirectory
    , siSegIdGen   = segIdGen
    , siSchema     = Map.empty
    , siSegments   = SegmentMap.empty
    , siSegRefs    = SegmentMap.empty
    }
