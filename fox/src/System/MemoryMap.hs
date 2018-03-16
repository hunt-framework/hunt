-- stolen from https://github.com/haskell-foundation/foundation/blob/b74b9d9a2364892e30289beffb5bd20c1575958f/foundation/Foundation/Foreign/MemoryMap/Posix.hsc

#include <sys/mman.h>
#include <unistd.h>

{-# LANGUAGE CPP, ForeignFunctionInterface #-}
module System.MemoryMap where

import System.Posix.Types
import Foreign.Ptr
import Foreign.C.Error
import Data.Bits

import           GHC.IO.FD
import           GHC.IO.IOMode
import qualified GHC.IO.Device as IO

foreign import ccall unsafe "mmap"
    c_mmap :: Ptr a -> CSize -> CInt -> CInt -> CInt -> COff -> IO (Ptr a)

foreign import ccall unsafe "munmap"
    c_munmap :: Ptr a -> CSize -> IO CInt

#if defined(POSIX_MADV_NORMAL)
foreign import ccall unsafe "posix_madvise"
    c_madvise :: Ptr a -> CSize -> CInt -> IO CInt
#else
foreign import ccall unsafe "madvise"
    c_madvise :: Ptr a -> CSize -> CInt -> IO CInt
#endif

foreign import ccall unsafe "msync"
    c_msync :: Ptr a -> CSize -> CInt -> IO CInt

foreign import ccall unsafe "mprotect"
    c_mprotect :: Ptr a -> CSize -> CInt -> IO CInt

#ifndef __HAIKU__
foreign import ccall unsafe "mlock"
    c_mlock :: Ptr a -> CSize -> IO CInt
#else
c_mlock :: Ptr a -> CSize -> IO CInt
c_mlock _ _ = return (-1)
#endif

#ifndef __HAIKU__
foreign import ccall unsafe "munlock"
    c_munlock :: Ptr a -> CSize -> IO CInt
#else
c_munlock :: Ptr a -> CSize -> IO CInt
c_munlock _ _ = return (-1)
#endif

foreign import ccall unsafe "sysconf"
    c_sysconf :: CInt -> CLong

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

cvalueOfMemorySync :: [MemorySyncFlag] -> CInt
cvalueOfMemorySync = foldl' (.|.) 0 . fmap toSync
  where toSync MemorySyncAsync      = (#const MS_ASYNC)
        toSync MemorySyncSync       = (#const MS_SYNC)
        toSync MemorySyncInvalidate = (#const MS_INVALIDATE)


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
