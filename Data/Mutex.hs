module Data.Mutex
    ( Mutex
    , new
    , lock
    , unlock
    ) where

import Control.Concurrent.MVar (MVar, newMVar, putMVar, takeMVar)

newtype Mutex = M { unMutex :: MVar () }

new :: IO Mutex
new = M `fmap` newMVar ()

lock :: Mutex -> IO ()
lock = takeMVar . unMutex

unlock :: Mutex -> IO ()
unlock m = putMVar (unMutex m) ()
