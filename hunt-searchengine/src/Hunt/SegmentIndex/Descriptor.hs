{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
module Hunt.SegmentIndex.Descriptor where

import           Hunt.Common.BasicTypes
import           Hunt.Scoring.Keys
import           Hunt.SegmentIndex.Types.Index
import           Hunt.SegmentIndex.Types.TermInfo
import           Hunt.Utility

import qualified Data.StringMap.Strict            as StringMap
import           Data.Text                        (Text)
import qualified Data.Text as Text

import           Prelude                          hiding (Word)

data IndexDescriptor =
  forall a. IndexDescriptor { idRepr    :: Word -> a
                            , idBuilder :: IndexBuilder a
                            }

data IndexBuilder a where
  IndexBuilder :: IO x
               -> (x -> a -> TermInfo -> IO x)
               -> (x -> IO (Index a))
               -> IndexBuilder a

textIndexBuilder :: IndexBuilder Text
textIndexBuilder = IndexBuilder start step stop
  where
    start        = pure $! StringMap.empty
    step sm s ti = pure $! StringMap.insert (Text.unpack s) ti sm
    stop sm = pure $! mkIndex sm

    mkIndex sm = undefined
 {-      Index { ixSearch = \t k -> do
                let
                  toL      = StringMap.toListShortestFirst
                  luCase   = toL .:: StringMap.lookupNoCase
                  pfCase   = toL .:: StringMap.prefixFilter
                  pfNoCase = toL .:: StringMap.prefixFilterNoCase

                return $ case t of
                  Case         -> case StringMap.lookup (Text.unpack k) pt of
                                    Nothing -> []
                                    Just xs -> [(k,xs)]
                  NoCase       -> luCase k pt
                  PrefixCase   -> pfCase k pt
                  PrefixNoCase -> pfNoCase k pt
            , ixSearchSc = \t k -> do
                addDefScore <$> undefined
            }
-}
