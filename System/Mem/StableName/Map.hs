{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE Trustworthy #-}
#elif __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Unsafe #-}
#endif
module System.Mem.StableName.Map
    ( Map
    , Representational
    , empty
    , null
    , singleton
    , member
    , notMember
    , insert
    , insertWith
    , insertWith'
    , adjust
    , adjust'
    , lookup
    , find
    , findWithDefault
    ) where

import System.Mem.StableName.Map.Internal
import Prelude hiding (null, lookup)
