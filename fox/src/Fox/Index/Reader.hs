module Fox.Index.Reader (
    IndexReader(..)
  , IxRdrEnv(..)

  , searchTerm
  ) where

import qualified Fox.Index.Directory as Directory
import qualified Fox.Index.InvertedFile as InvertedFile
import qualified Fox.Index.Segment as Segment
import qualified Fox.Types.Occurrences as Occurrences
import qualified Fox.Types.SegmentMap as SegmentMap
import qualified Fox.Types.Token as Token

import qualified Data.Key as Key

data IxRdrEnv
  = IxRdrEnv {
        ixrIndexDir :: !Directory.IndexDirectory
      , ixrSegments :: !(SegmentMap.SegmentMap Segment.Segment)
      }

newtype IndexReader a
  = IndexReader { runIndexReader :: IxRdrEnv -> IO a }

instance Functor IndexReader where
  fmap = mapIndexReader

instance Applicative IndexReader where
  pure  = pureIndexReader
  (<*>) = apIndexReader

instance Monad IndexReader where
  (>>=) = bindIndexReader

mapIndexReader
  :: (a -> b)
  -> IndexReader a
  -> IndexReader b
mapIndexReader f (IndexReader m) =
  IndexReader $ \r -> fmap f (m r)

pureIndexReader :: a -> IndexReader a
pureIndexReader a =
  IndexReader $ \_ -> pure a

apIndexReader
  :: IndexReader (a -> b)
  -> IndexReader a
  -> IndexReader b
apIndexReader (IndexReader mf) (IndexReader ma) =
  IndexReader $ \r -> mf r <*> ma r

bindIndexReader
  :: IndexReader a
  -> (a -> IndexReader b)
  -> IndexReader b
bindIndexReader (IndexReader m) f =
  IndexReader (\r -> do a <- m r
                        runIndexReader (f a) r)

asks :: (IxRdrEnv -> a) -> IndexReader a
asks f = IndexReader (\r -> pure (f r))

runIfM :: InvertedFile.IfM a -> IndexReader a
runIfM m = IndexReader (\_ -> m)

searchTerm
  :: InvertedFile.TextSearchOp
  -> Token.Term
  -> IndexReader Occurrences.Occurrences
searchTerm searchOp term = do
  indexDir <- asks ixrIndexDir
  segments <- asks ixrSegments

  runIfM $ do
    occs <- Key.forWithKey segments $ \segmentId segment -> do
      let
        segmentDirLayout =
          Directory.segmentDirLayout indexDir segmentId
      Segment.searchTerm segmentDirLayout segment searchOp term
    return (mconcat (SegmentMap.elems occs))
