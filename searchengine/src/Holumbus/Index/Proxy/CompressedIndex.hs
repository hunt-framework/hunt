module Holumbus.Index.Proxy.CompressedIndex where

{--
 -
 - there is an implementation for a compreossed index
 - within the InvertedIndex implementation.
 -
 - would be great to have a more general solution as
 - proxy implementation here.
 -
 -}


{--
import           Data.ByteString          (ByteString)
import           Holumbus.Data.PrefixTree (PrefixTree)
import qualified Holumbus.Data.PrefixTree as PT
import           Holumbus.Index.Index

import           Control.Applicative ((<$>))
import           Control.Arrow       (first, second)
--}


{-

newtype CompressedIndex impl v = CompressedIx (impl ByteString)

instance Index (CompressedIndex impl) where
    type IKey (CompressedIndex impl) v = IKey impl ByteString
    type IToL (CompressedIndex impl) v = IToL impl v
    type ICon (CompressedIndex impl) v = ( Index impl
                                         , Compression v
                                         , IVal impl v ~ ByteString
                                         , IVal impl ByteString ~ ByteString
                                         , IKey impl v ~ IKey impl ByteString
                                         , ICon impl v
                                         , ICon impl (IVal impl v)
                                         , IType (CompressedIndex impl) v ~ IType impl v
                                         )


    insert k v (CompressedIx m) = CompressedIx $ insert k (compress v) m
    delete k   (CompressedIx m) = CompressedIx $ delete k              m
    empty                       = CompressedIx $ empty
    fromList xs                 = CompressedIx $ fromList $ map (second compress) xs
    search k   (CompressedIx m) = second decompress <$> search k m
    toList     (CompressedIx m) = second decompress <$> toList m
--}


{--
newtype CompStrMap v = CM (StringMap ByteString)
    deriving (Show)

instance Index CompStrMap where
    type IKey CompStrMap v = String
    type IVal CompStrMap v = v
    type IToL CompStrMap v = [(String, v)]
    type ICon CompStrMap v = Compression v

--}
