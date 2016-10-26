{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}
module Hunt.SegmentIndex.Descriptor where

import           Hunt.Common.BasicTypes
import           Hunt.Scoring.Keys
import           Hunt.SegmentIndex.Types.Index
import           Hunt.SegmentIndex.Types.TermInfo
import           Hunt.Utility

import           Control.Arrow
import           Data.Foldable
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

runBuilder :: Foldable f => Builder a b -> f a -> IO b
runBuilder (Builder start step stop) xs = do
  s  <- start
  stop =<< foldlM step s xs
{-# INLINE runBuilder #-}

type IndexBuilder a = Builder (a, TermInfo) (Index a)

textIndexDescr :: IndexDescriptor
textIndexDescr = IndexDescriptor id textIndexBuilder

data T2 a b = T2 !a !b

textIndexBuilder :: IndexBuilder Text
textIndexBuilder = Builder start step stop
  where
    start                  = pure $! T2 0 StringMap.empty
    step (T2 n sm) (s, ti) =
      pure $! T2 (n + 1) (StringMap.insert (Text.unpack s) ti sm)
    stop (T2 n sm)         = pure $! mkIndex n sm

    mkIndex n sm =
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
               , ixNumTerms      = n
               }
