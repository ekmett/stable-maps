{-# LANGUAGE Unsafe #-}

-- | The API in "System.Mem.StableName.Map" allows @'Map' f@ only
-- when @f@'s parameter has a representational role. This module
-- exports functions that defeat that restriction. If it breaks,
-- you get to keep both pieces. In many cases,
-- "System.Mem.StableName.TypedMap" will do the trick, but it's
-- somewhat less efficient.
module System.Mem.StableName.Map.Unsafe
    ( unsafeEmpty
    , unsafeSingleton
    ) where

import System.Mem.StableName.Map.Internal
