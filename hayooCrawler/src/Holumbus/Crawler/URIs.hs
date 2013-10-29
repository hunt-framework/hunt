{-# OPTIONS #-}

-- ------------------------------------------------------------

module Holumbus.Crawler.URIs
where

import qualified Data.StringMap as SM

-- ------------------------------------------------------------

-- | An URI is represented as a String
type URI                        = String
type URIWithLevel               = (URI, Int)

-- | A set of URIs implemeted as a prefix tree. This implementation
-- is space efficient, because of many equal prefixes in the crawled set of URIs

type URIs                       = URIs' ()
type URIsWithLevel              = URIs' Int     -- URIs with a priority or clicklevel

type URIs' a                    = SM.StringMap a

-- ------------------------------------------------------------

emptyURIs               :: URIs' a
emptyURIs               = SM.empty

singletonURIs           :: URI -> URIs
singletonURIs           = flip SM.singleton ()

singletonURIs'          :: URI -> a -> URIs' a
singletonURIs'          = SM.singleton

nullURIs                :: URIs' a -> Bool
nullURIs                = SM.null

memberURIs              :: URI -> URIs' a -> Bool
memberURIs              = SM.member

cardURIs                :: URIs' a -> Int
cardURIs                = SM.size

nextURI                 :: URIs' a -> (URI, a)
nextURI                 = head . toListURIs'

nextURIs                :: Int -> URIs' a -> [(URI, a)]
nextURIs n              = take n . toListURIs'

insertURI               :: URI -> URIs  -> URIs
insertURI               = flip SM.insert ()

insertURI'              :: URI -> a -> URIs' a -> URIs' a
insertURI'              = SM.insert

deleteURI               :: URI -> URIs' a -> URIs' a
deleteURI               = SM.delete

deleteURIs              :: URIs' b -> URIs' a -> URIs' a
deleteURIs              = flip SM.difference

unionURIs               :: URIs -> URIs -> URIs
unionURIs               = SM.union

unionURIs'              :: (a -> a -> a) -> URIs' a -> URIs' a -> URIs' a
unionURIs'              = SM.unionWith

diffURIs                :: URIs' a -> URIs' a -> URIs' a
diffURIs                = SM.difference

fromListURIs            :: [URI] -> URIs
fromListURIs            = SM.fromList . map (\ x -> (x, ()))

fromListURIs'           :: [(URI, a)] -> URIs' a
fromListURIs'           = SM.fromList

toListURIs              :: URIs' a -> [URI]
toListURIs              = SM.keys                -- map fst . SM.toList

toListURIs'             :: URIs' a -> [(URI, a)]
toListURIs'             = SM.toList

foldURIs                :: (URI -> b -> b) -> b -> URIs -> b
foldURIs f              = SM.foldWithKey (\ x _ r -> f x r)

foldURIs'               :: (URI -> a -> b -> b) -> b -> URIs' a -> b
foldURIs'               = SM.foldWithKey

-- ------------------------------------------------------------


