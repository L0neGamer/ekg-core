{-# LANGUAGE CPP #-}

module System.Compat
#if !MIN_VERSION_base(4,8,0)
    ((<$!>))
#else
#endif
  where

-- | Import <$!> from base-4.8 for ghc 7 compat
#if !MIN_VERSION_base(4,8,0)
infixl 4 <$!>

-- | Strict version of 'Data.Functor.<$>'.
--
-- @since 4.8.0.0
(<$!>) :: Monad m => (a -> b) -> m a -> m b
{-# INLINE (<$!>) #-}
f <$!> m = do
  x <- m
  let z = f x
  z `seq` return z
#endif
