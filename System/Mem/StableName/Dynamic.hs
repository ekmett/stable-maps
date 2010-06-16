{-# LANGUAGE TypeFamilies, Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  System.Mem.StableName.Dynamic
-- Copyright   :  (c) Edward Kmett 2010
-- License     :  BSD3
-- Maintainer  :  ekmett@gmail.com
-- Stability   :  experimental
-- Portability :  GHC only
--
-- Dynamic stable names are a way of performing fast (O(1)), not-quite-exact comparison between objects.
--
-- Dynamic stable names solve the following problem: suppose you want to build a hash table with Haskell objects as keys, but you want to use pointer equality for comparison; maybe because the keys are large and hashing would be slow, or perhaps because the keys are infinite in size. We can't build a hash table using the address of the object as the key, because objects get moved around by the garbage collector, meaning a re-hash would be necessary after every garbage collection.
-----------------------------------------------------------------------------

module System.Mem.StableName.Dynamic 
    ( DynamicStableName(..)
    , hashDynamicStableName
    , makeDynamicStableName
    , wrapStableName
    ) where

import GHC.Prim

import System.Mem.StableName (StableName, makeStableName, hashStableName)
import Unsafe.Coerce (unsafeCoerce)

{-|
  An abstract name for an object, that supports equality and hashing.

  Dynamic stable names have the following property:

  * If @sn1 :: DynamicStableName@ and @sn2 :: DynamicStableName@ and @sn1 == sn2@
   then @sn1@ and @sn2@ were created by calls to @makeStableName@ on 
   the same object.

  The reverse is not necessarily true: if two dynamic stable names are not
  equal, then the objects they name may still be equal.  Note in particular
  that `makeDynamicStableName` may return a different `DynamicStableName` 
  after an object is evaluated.

  Dynamic Stable Names are similar to Stable Pointers ("Foreign.StablePtr"),
  but differ in the following ways:

  * There is no @freeDynamicStableName@ operation, unlike "Foreign.StablePtr"s.
    Dynamic Stable Names are reclaimed by the runtime system when they are no
    longer needed.

  * There is no @deRefDynamicStableName@ operation.  You can\'t get back from
    a dynamic stable name to the original Haskell object.  The reason for
    this is that the existence of a stable name for an object does not
    guarantee the existence of the object itself; it can still be garbage
    collected.

-}

newtype DynamicStableName = DynamicStableName (StableName Any)

-- | Makes a 'DynamicStableName' for an arbitrary object.  The object passed as
-- the first argument is not evaluated by 'makeDynamicStableName'.
makeDynamicStableName :: t -> IO DynamicStableName
makeDynamicStableName a = do
    s <- makeStableName a
    return (wrapStableName s)

-- | Convert a 'DynamicStableName' to an 'Int'.  The 'Int' returned is not
-- necessarily unique; several 'DynamicStableName's may map to the same 'Int'
-- (in practice however, the chances of this are small, so the result
-- of 'hashDynamicStableName' makes a good hash key).
hashDynamicStableName :: DynamicStableName -> Int
hashDynamicStableName (DynamicStableName sn) = hashStableName sn

instance Eq DynamicStableName where
    DynamicStableName sn1 == DynamicStableName sn2 = sn1 == sn2

wrapStableName :: StableName a -> DynamicStableName
wrapStableName s = DynamicStableName (unsafeCoerce s)
