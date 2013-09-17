{-# LANGUAGE DataKinds            #-}
-- {-# LANGUAGE EmptyDataDecls     #-}
-- {-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE TypeSynonymInstances  #-}
-- {-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverloadedStrings     #-}
-- {-# LANGUAGE Rank2Types            #-}

module AssocTypes where

import           Control.Applicative ((<$>))
import           Control.Arrow       (first, second)

-- ----------------------------------------

class Index i where
    type IKey i :: *
    type IVal i :: *
    type IPairs i :: *
    type IPairs i = [(IKey i, IVal i)]

    insert      :: IKey i -> IVal i    -> i -> i
    delete      :: IKey i              -> i -> i
    empty       ::                        i
    fromList    :: [(IKey i, IVal i)]  -> i
    toList      ::                        i -> IPairs i
    search      :: IKey i              -> i -> Maybe (IVal i)

-- ----------------------------------------

type Map k v = [(k, v)]		-- dummy for Data.Map

newtype StringMap v = SM (Map String v)
    deriving (Show)

instance Index (StringMap v) where
    type IKey (StringMap v) = String
    type IVal (StringMap v) = v

    insert k v (SM m) = SM $ (k, v) : m
    delete k   (SM m) = SM $ filter ((/= k) . fst) m
    empty             = SM []
    fromList xs       = SM xs
    search k   (SM m) = lookup k m
    toList     (SM m) = m

-- ----------------------------------------

newtype Text = T String		-- dummy for Data.Text

type ISO' a b = (a -> b, b -> a)

isoTextString :: ISO' Text String
isoTextString = (\ (T s) -> s, T)

unpack :: Text -> String
unpack = fst isoTextString

pack :: String -> Text
pack   = snd isoTextString

newtype TextMap v = TM (StringMap v)
    deriving (Show)

instance Index (TextMap v) where
    type IKey (TextMap v) = Text
    type IVal (TextMap v) = v

    insert k v (TM m) = TM $ insert (unpack k) v m
    delete k   (TM m) = TM $ delete (unpack k)   m
    empty             = TM empty
    fromList xs       = TM $ fromList $ map (first unpack) xs
    search k   (TM m) = search (unpack k) m
    toList     (TM m) = first pack <$> toList m

-- ----------------------------------------
--
-- example: value of a map is transformed into something else

newtype ByteString = BS String		-- dummy for ByteString
    deriving (Show)

class Compression a where               -- dummy for serialisation and compression
    compress   :: a -> ByteString
    decompress :: ByteString -> a

newtype CompStrMap v = CM (StringMap ByteString)
    deriving (Show)

instance (Compression v) => Index (CompStrMap v) where
    type IKey (CompStrMap v) = String
    type IVal (CompStrMap v) = v

    insert k v (CM m) = CM $ insert k (compress v) m
    delete k   (CM m) = CM $ delete k              m
    empty             = CM $ empty
    fromList xs       = CM $ fromList $ map (second compress) xs
    search k   (CM m) = decompress <$> search k m
    toList     (CM m) = second decompress <$> toList m

-- ----------------------------------------
--
-- values

instance Compression Int where
    compress = BS . show
    decompress (BS s) = read s

xs1 :: [(String, Int)]
xs1 = [("abc", 1), ("xyz", 2), ("123", 3)]

sm1 :: StringMap Int
sm1 = fromList xs1

tm1 :: TextMap Int
tm1 = fromList $ map (first pack) xs1

cm1 :: CompStrMap Int
cm1 = fromList xs1


