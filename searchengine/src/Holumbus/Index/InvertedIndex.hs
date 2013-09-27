module Holumbus.Index.InvertedIndex where

import           Data.Text                      (pack, unpack)
import qualified Data.IntSet                    as IS
import           Control.Arrow                  (first, second)
import           Control.Applicative            ((<$>))

import           Holumbus.Data.PrefixTree       (PrefixTree)
import qualified Holumbus.Data.PrefixTree       as PT
import           Holumbus.Index.Index
import           Holumbus.Index.Proxy.ContextIndex
import           Holumbus.Index.Common   
import qualified Holumbus.Index.Common.DocIdMap        as DM
import qualified Holumbus.Index.Common.Occurrences     as Occ       
import           Holumbus.Index.Common.Compression hiding (delete)
import qualified Holumbus.DocTable.DocTable            as DT
------------------------------------------------------------------------
-- index implementation for simple prefixtreee

newtype DmPrefixTree v = DmPT (PrefixTree v)
    deriving Show

instance Index DmPrefixTree where
    type IKey DmPrefixTree v = PT.Key
    type IVal DmPrefixTree v = v
  
    insert k v (DmPT pt) 
        = DmPT $ PT.insert k v pt

    batchDelete ks (DmPT pt) = undefined
-- xxx TODO how to enforce the key to be DocMapId? 
--        = DmPT $ PT.map (\m -> DM.diffWithSet m ks) pt

    empty                    
        = DmPT $ PT.empty

    fromList                 
        = (DmPT . PT.fromList)

    toList (DmPT pt) 
        = PT.toList pt

    -- | xxx TODO use indextype parameter for real search
    search t k (DmPT pt) 
        = case t of 
            _ -> PT.prefixFindWithKey k pt

    unionWith op (DmPT pt1) (DmPT pt2) 
        = DmPT $ PT.unionWith op pt1 pt2

-------------------------------------------------------------------------
-- setting compressed prefixtree ontop of simple prefixtree index 

newtype ComprPrefixTree v cv = ComprPT { comprPT :: DmPrefixTree cv}
    deriving Show

instance Index (ComprPrefixTree v) where
    type IKey (ComprPrefixTree v) cv = PT.Key
    type IVal (ComprPrefixTree v) cv = v
    type ICon (ComprPrefixTree v) cv = (Compression (IVal (ComprPrefixTree v) cv) cv)
 
    insert k v (ComprPT i)     
        = ComprPT $ insert k (compress v) i
   
    batchDelete ks (ComprPT i)     
        = ComprPT $ batchDelete ks i 
   
    empty                              
        = ComprPT $ empty
   
    fromList l                         
        = ComprPT . fromList $ map (second compress) l
   
    toList (ComprPT i)     
        = second decompress <$> toList i
   
    search t k (ComprPT i)               
        = second decompress <$> search t k i
   
    unionWith op (ComprPT i1) (ComprPT i2) 
        = ComprPT $ unionWith (\o1 o2 -> compress $ op (decompress o1) (decompress o2)) i1 i2

-------------------------------------------------------------------------
-- setting inverted index with enforced val ~ Occurrences ontop of 
-- compr prefixtree 

type ContextInvertedIndex = ContextIndex InvertedIndex Occurrences

newtype InvertedIndex v = InvIx { invIx :: ComprPrefixTree Occurrences CompressedOccurrences }
  deriving Show

instance Compression Occurrences CompressedOccurrences where
    compress   = deflateOcc
    decompress = inflateOcc

instance Index InvertedIndex where
    type IKey   InvertedIndex v = Word
    type IVal   InvertedIndex v = Occurrences
    type ICon   InvertedIndex v = (v ~ Positions)
 
    insert w o                  
        = unionWith Occ.merge (singleton w o)

    batchDelete docIds (InvIx (ComprPT pt)) 
        = InvIx $ ComprPT $ unionWith (\os _ -> differenceWithKeySet docIds os) pt empty
  
    empty                       
        = InvIx $ empty
   
    fromList l                  
        = InvIx $ fromList $ map (first unpack) l
   
    toList (InvIx i)  
        = first pack <$> toList i
   
    search t k (InvIx i)  
        = first pack <$> search t (unpack k) i
   
    unionWith op (InvIx i1) (InvIx i2) 
        = InvIx $ unionWith op i1 i2

-- | Single word InvertedIndex
singleton w o = InvIx $ (insert (unpack w) o empty) 
