{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
module Hunt.SegmentIndex.Descriptor where

import           Hunt.Common.BasicTypes
import           Hunt.Scoring.Keys
import           Hunt.SegmentIndex.Types.Index
import           Hunt.SegmentIndex.Types.TermInfo
import           Hunt.Utility

import           Control.Arrow
import qualified Data.StringMap.Strict            as StringMap
import           Data.Text                        (Text)
import qualified Data.Text                        as Text

import           Prelude                          hiding (Word)

data IndexDescriptor =
  forall a. IndexDescriptor { idRepr    :: Word -> a
                            , idBuilder :: IndexBuilder a
                            }

data Builder a b where
  Builder :: IO x
          -> (x -> a -> IO x)
          -> (x -> IO b)
          -> Builder a b

type IndexBuilder a = Builder (a, TermInfo) (Index a)

textIndexBuilder :: IndexBuilder Text
textIndexBuilder = Builder start step stop
  where
    start           = pure $! StringMap.empty
    step sm (s, ti) = pure $! StringMap.insert (Text.unpack s) ti sm
    stop sm         = pure $! mkIndex sm

    mkIndex sm =
      let
        search :: TextSearchOp -> Text -> IO [(Text, TermInfo)]
        search t k = do
          let
            toL      = fmap (first Text.pack) . StringMap.toListShortestFirst
            luCase   = toL .:: StringMap.lookupNoCase
            pfCase   = toL .:: StringMap.prefixFilter
            pfNoCase = toL .:: StringMap.prefixFilterNoCase

          return $ case t of
            Case         -> case StringMap.lookup (Text.unpack k) sm of
                              Nothing -> []
                              Just xs -> [(k, xs)]
            NoCase       -> luCase (Text.unpack k) sm
            PrefixCase   -> pfCase (Text.unpack k) sm
            PrefixNoCase -> pfNoCase (Text.unpack k) sm

        lookupRange :: Text -> Text -> IO [(Text, TermInfo)]
        lookupRange t1 t2 =
          pure
          $ fmap (first Text.pack)
          $ StringMap.toList
          $ StringMap.lookupRange (Text.unpack t1) (Text.unpack t2) sm

      in Index { ixSearch        = search
               , ixSearchSc      = \t k -> addDefScore <$> search t k
               , ixLookupRange   = lookupRange
               , ixLookupRangeSc = \t1 t2 -> addDefScore <$> lookupRange t1 t2
               }
