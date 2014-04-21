{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}
-- | An atomic integer value. All operations are thread safe.
module Data.Atomic
    (
      Atomic
    , new
    , read
    , write
    , inc
    , dec
    , add
    , subtract
    ) where

import Data.Int (Int64)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)
import Prelude hiding (read, subtract)

-- | A mutable, atomic integer.
newtype Atomic = C (ForeignPtr Int64)

-- | Create a new, zero initialized, atomic.
new :: Int64 -> IO Atomic
new n = do
    fp <- mallocForeignPtr
    withForeignPtr fp $ \ p -> poke p n
    return $ C fp

read :: Atomic -> IO Int64
read (C fp) = withForeignPtr fp cRead

foreign import ccall unsafe "hs_atomic_read" cRead :: Ptr Int64 -> IO Int64

-- | Set the atomic to the given value.
write :: Atomic -> Int64 -> IO ()
write (C fp) n = withForeignPtr fp $ \ p -> cWrite p n

foreign import ccall unsafe "hs_atomic_write" cWrite
    :: Ptr Int64 -> Int64 -> IO ()

-- | Increase the atomic by one.
inc :: Atomic -> IO ()
inc atomic = add atomic 1

-- | Decrease the atomic by one.
dec :: Atomic -> IO ()
dec atomic = subtract atomic 1

-- | Increase the atomic by the given amount.
add :: Atomic -> Int64 -> IO ()
add (C fp) n = withForeignPtr fp $ \ p -> cAdd p n

-- | Decrease the atomic by the given amount.
subtract :: Atomic -> Int64 -> IO ()
subtract (C fp) n = withForeignPtr fp $ \ p -> cSubtract p n

-- | Increase the atomic by the given amount.
foreign import ccall unsafe "hs_atomic_add" cAdd :: Ptr Int64 -> Int64 -> IO ()

-- | Increase the atomic by the given amount.
foreign import ccall unsafe "hs_atomic_subtract" cSubtract
    :: Ptr Int64 -> Int64 -> IO ()
