module Hunt.SegmentIndex.Write where

import qualified Data.Store        as Store
import           Data.Text         (Text)
import qualified Data.Text.Foreign as Text
import           Data.Word
import           Foreign.Ptr

type Offset = Int

newtype UTF16 = UTF16 { unUTF16 :: Text }

instance Store.Store UTF16 where
  size = Store.VarSize $ \s -> 2 * Text.lengthWord16 (unUTF16 s)
  {-# INLINE size #-}

  poke (UTF16 s) = Store.Poke $ \op off -> do
    case ((castPtr op :: Ptr Word8) `plusPtr` off) of
      op' -> do Text.unsafeCopyToPtr s (castPtr op')
                return (off + 2 * Text.lengthWord16 s)
  {-# INLINE poke #-}




newtype Varint a = Varint { unVarint :: a }

data TermInfo =
  TermInfo { tiPrefixLen :: !(Varint Int)
           , tiSuffixLen :: !(Varint Int)
           , tiTerm      :: !UTF16
           , tiOccCount  :: !(Varint Int)
           , tiOccOffset :: !(Varint Offset)
           }
