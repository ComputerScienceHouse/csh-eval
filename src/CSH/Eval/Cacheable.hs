{-|
Module      : CSH.Eval.Cacheable
Description : The 'Cacheable' monad.
Copyright   : Stephen Demos, Matt Gambogi, Travis Whitaker, Computer Science House 2015
License     : MIT
Maintainer  : pvals@csh.rit.edu
Stability   : Provisional
Portability : POSIX

CSH.Eval.Cacheable defines the 'Cacheable' monad and its primitives.
-}

module CSH.Eval.Cacheable where

type Cacheable a = IO (Either String a)
