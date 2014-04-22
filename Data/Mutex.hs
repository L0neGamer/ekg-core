module Data.Mutex
    ( Mutex
    , new
    , lock
    , unlock
    ) where

import Data.Int (Int64)
import Foreign.ForeignPtr (ForeignPtr, mallocForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (poke)

newtype Mutex = M { unM :: ForeignPtr Int64 }

new :: IO Mutex
new = do
    fp <- mallocForeignPtr
    withForeignPtr fp $ \ p -> poke p 0
    return $ M fp

lock :: Mutex -> IO ()
lock m = withForeignPtr (unM m) cLock

foreign import ccall unsafe "hs_lock" cLock :: Ptr Int64 -> IO ()

unlock :: Mutex -> IO ()
unlock m = withForeignPtr (unM m) cUnlock

foreign import ccall unsafe "hs_unlock" cUnlock :: Ptr Int64 -> IO ()
