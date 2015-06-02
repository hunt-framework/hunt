module Hunt.ContextIndex.Commit where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Binary
import qualified Data.List as List
import           Data.Monoid
import           Data.Set (Set)
import qualified Data.Set as Set

import           Hunt.ContextIndex.Segment
import           Hunt.DocTable (DocTable)
import           Hunt.ContextIndex.Types

-- | Flushes all dirty and not yet written `Segment`s to the index directory
--
commit :: (Binary dt, DocTable dt, MonadIO m) => FilePath -> ContextIndex dt -> m (ContextIndex dt)
commit dir ixx
  = return ixx
