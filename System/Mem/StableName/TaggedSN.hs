{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies, Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
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

module System.Mem.StableName.TaggedSN
    ( TaggedSN(..)
    , hashTaggedSN
    , makeTaggedSN
    , wrapTaggedSN
    ) where

import GHC.Exts (Any)

import System.Mem.StableName (StableName, makeStableName, hashStableName)
import Unsafe.Coerce (unsafeCoerce)
import Data.Hashable
import GHC.Fingerprint (Fingerprint)
import Data.Typeable (Typeable, typeRep, typeRepFingerprint)
import Data.Proxy (Proxy (..))

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

data TaggedSN = TaggedSN {-# UNPACK #-} !Fingerprint !(StableName Any)
  deriving Eq

instance Hashable TaggedSN where
  -- This could produce collisions when the same value
  -- has multiple types, but I don't know that it's worth
  -- the performance cost in the common case to avoid that.
  hashWithSalt s (TaggedSN _fp sn) = hashWithSalt s sn

instance Show TaggedSN where
  showsPrec d (TaggedSN fp sn) = showParen (d > 10) $
    showString "TaggedSN {fp = " .
    showsPrec 11 fp .
    showString ", hash = " .
    showsPrec 11 (hashStableName sn) .
    showString "}"

-- | Makes a 'DynamicStableName' for an arbitrary object.  The object passed as
-- the first argument is not evaluated by 'makeDynamicStableName'.
makeTaggedSN :: Typeable t => t -> IO TaggedSN
makeTaggedSN a = do
    s <- makeStableName a
    return (wrapTaggedSN s)

-- | Convert a 'TaggedSN' to an 'Int'.  The 'Int' returned is not
-- necessarily unique; several 'TaggedSN's may map to the same 'Int'
-- (in practice however, the chances of this are small, so the result
-- of 'hashTaggedSN' makes a good hash key).
hashTaggedSN :: TaggedSN -> Int
hashTaggedSN (TaggedSN _ sn) = hashStableName sn

wrapTaggedSN :: forall a. Typeable a => StableName a -> TaggedSN
wrapTaggedSN s = TaggedSN (typeRepFingerprint (typeRep (Proxy :: Proxy a))) (unsafeCoerce s)
