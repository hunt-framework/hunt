#include <sys/mman.h>
#include <unistd.h>

{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module System.MemoryMap (
    FileMapping(..)
  , fileMapRead
  )where

import Control.Exception
import GHC.IO.FD
import GHC.IO.IOMode
import Data.Bits
import Data.List (foldl')
import Data.Word (Word8, Word64)
import Foreign.C.Error
import Foreign.C.Types
import Foreign.Ptr
import System.Posix.Types
import qualified GHC.IO.Device as IO

foreign import ccall unsafe "mmap"
    c_mmap :: Ptr a -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr a)

foreign import ccall unsafe "munmap"
    c_munmap :: Ptr a -> CSize -> IO CInt

-- | Mapping flag
data MemoryMapFlag =
      MemoryMapShared  -- ^ memory changes are shared between process
    | MemoryMapPrivate -- ^ memory changes are private to process
    deriving (Show,Eq)

-- | Memory protection
data MemoryProtection =
      MemoryProtectionNone
    | MemoryProtectionRead
    | MemoryProtectionWrite
    | MemoryProtectionExecute
    deriving (Show,Eq)

-- | Advice to put on memory.
--
-- only define the posix one.
data MemoryAdvice =
      MemoryAdviceNormal     -- ^ no specific advice, the default.
    | MemoryAdviceRandom     -- ^ Expect page references in random order. No readahead should occur.
    | MemoryAdviceSequential -- ^ Expect page references in sequential order. Page should be readahead aggressively.
    | MemoryAdviceWillNeed   -- ^ Expect access in the near future. Probably a good idea to readahead early
    | MemoryAdviceDontNeed   -- ^ Do not expect access in the near future.
    deriving (Show,Eq)

-- | Memory synchronization flags
data MemorySyncFlag =
      MemorySyncAsync      -- ^ perform asynchronous write.
    | MemorySyncSync       -- ^ perform synchronous write.
    | MemorySyncInvalidate -- ^ invalidate cache data.
    deriving (Show,Eq)

cvalueOfMemoryProts :: [MemoryProtection] -> CInt
cvalueOfMemoryProts = foldl' (.|.) 0 . fmap toProt
  where toProt :: MemoryProtection -> CInt
        toProt MemoryProtectionNone    = (#const PROT_NONE)
        toProt MemoryProtectionRead    = (#const PROT_READ)
        toProt MemoryProtectionWrite   = (#const PROT_WRITE)
        toProt MemoryProtectionExecute = (#const PROT_EXEC)

-- | Map pages of memory.
--
-- If fd is present, this memory will represent the file associated.
-- Otherwise, the memory will be an anonymous mapping.
--
-- use 'mmap'
memoryMap :: Maybe (Ptr a)      -- ^ The address to map to if MapFixed is used.
          -> CSize              -- ^ The length of the mapping
          -> [MemoryProtection] -- ^ the memory protection associated with the mapping
          -> MemoryMapFlag      -- ^
          -> Maybe Fd
          -> COff
          -> IO (Ptr a)
memoryMap initPtr sz prots flag mfd off =
    throwErrnoIf (== m1ptr) "mmap" (c_mmap (maybe nullPtr id initPtr) sz cprot cflags fd off)
  where m1ptr  = nullPtr `plusPtr` (-1)
        fd     = maybe (-1) (\(Fd v) -> v) mfd
        cprot  = cvalueOfMemoryProts prots
        cflags = maybe cMapAnon (const 0) mfd
             .|. maybe 0 (const cMapFixed) initPtr
             .|. toMapFlag flag

#ifdef __APPLE__
        cMapAnon  = (#const MAP_ANON)
#else
        cMapAnon  = (#const MAP_ANONYMOUS)
#endif
        cMapFixed = (#const MAP_FIXED)

        toMapFlag MemoryMapShared  = (#const MAP_SHARED)
        toMapFlag MemoryMapPrivate = (#const MAP_PRIVATE)

-- | Unmap pages of memory
--
-- use 'munmap'
memoryUnmap :: Ptr a -> CSize -> IO ()
memoryUnmap ptr sz = throwErrnoIfMinus1_ "munmap" (c_munmap ptr sz)

-- | Contains all the information related to a file mapping,
-- including the size and the finalizer function.
data FileMapping = FileMapping
    { fileMappingPtr   :: !(Ptr Word8)
    , fileMappingSize  :: !Word64
    , fileMappingUnmap :: IO ()
    }

fileMapRead :: FilePath -> IO FileMapping
fileMapRead fp =
  bracket (openFile fp ReadMode True) (IO.close . fst) $ \(fd,_) -> do
    sz   <- IO.getSize fd
    let csz = fromIntegral sz
    p    <- memoryMap Nothing csz [MemoryProtectionRead] MemoryMapPrivate (Just $ Fd $ fdFD fd) 0
    return $! FileMapping p (fromIntegral sz) (memoryUnmap p csz)
