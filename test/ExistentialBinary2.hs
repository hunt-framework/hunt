{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE ExistentialQuantification #-}

--import GHC.AssertNF
--import Control.DeepSeq
import           Control.Applicative
import           Data.Binary
import qualified Data.List              as L
import           Data.Map.Strict        (Map)
import qualified Data.Map.Strict        as M
import           Data.Typeable
import           Data.Typeable.Internal (TyCon (..), TypeRep (..))
import           GHC.Fingerprint.Type   (Fingerprint (..))

-- index implementations
class IsIndex x where
  show' :: x -> String

data Impl1 = Ix1 { ix1 :: Map Int String }
  deriving (Typeable, Eq, Show)

instance IsIndex Impl1 where
  show' (Ix1 x) = show x

instance Binary Impl1 where
  put = put . ix1
  get = get >>= return . Ix1

data Impl2 = Ix2 { ix2 :: [String] }
  deriving (Typeable, Eq, Show)

instance IsIndex Impl2 where
  show' (Ix2 x) = show x

instance Binary Impl2 where
  put = put . ix2
  get = get >>= return . Ix2

-- the existential container
data Index = forall a . (IsIndex a, Typeable a, Binary a) => Index a


-- general typeable binary instances
instance Binary Fingerprint where
  put (Fingerprint hi lo) = put hi >> put lo
  get = Fingerprint <$> get <*> get

instance Binary TypeRep where
  put (TypeRep fp tyCon ts) = put fp >> put tyCon >> put ts
  get = TypeRep <$> get <*> get <*> get

instance Binary TyCon where
  put (TyCon hash package modul name) = put hash >> put package >> put modul >> put name
  get = TyCon <$> get <*> get <*> get <*> get


-- list of all current index implementations
ix :: [Index]
ix = [ Index $ Ix1 M.empty
     , Index $ Ix2 []
     ]

-- existential types binary instnace
instance Binary Index where
  put (Index x) = put (typeOf x) >> put x
  get = do
     t <- get
     case L.find (\(Index i) -> t == typeOf i) ix  of
       Just (Index x) -> Index <$> get `asTypeOf` return x
       Nothing        -> error $ "Unable to load index of type: " ++ show t

------------------------------------------------------------------------
-- existential type index

------------------------------------------------------------------------
-- test code


main :: IO ()
main = do
  let i1 = mkIx1
  let i2 = mkIx2
  putStrLn "Before serialization/deserialisation"
  print . show'' $ i1
  print . show'' $ i2
  execStore file1 i1
  execStore file2 i2
  i3 <- execLoad file1
  i4 <- execLoad file2
  putStrLn "After Store => Load"
  print . show'' $ i3
  print . show'' $ i4
  return ()
  where
  file1 = "/tmp/ix1"
  file2 = "/tmp/ix2"

execStore :: Binary a => FilePath -> a -> IO ()
execStore filename x = do
    encodeFile filename x
    return ()

execLoad :: Binary a => FilePath -> IO a
execLoad = decodeFile

-- unpack index and run show
show'' :: Index -> String
show'' (Index x) = show' x

-- mk index with impl1
mkIx1 :: Index
mkIx1 = Index $ i1
  where
  i1 :: Impl1
  i1 = Ix1 $ M.fromList [(1, "test"), (2, "test2")]

-- mk index with impl2
mkIx2 :: Index
mkIx2 = Index $ i1
  where
  i1 :: Impl2
  i1 = Ix2 ["asd", "xyz"]


--buildIx :: Map String Index
--buildIx = M.fromList [("ix1", mkIx1), ("ix2", mkIx2)]
