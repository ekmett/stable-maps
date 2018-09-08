{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Trustworthy #-}
#endif
#if __GLASGOW_HASKELL__ >= 707
{-# LANGUAGE RoleAnnotations #-}
#endif
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | @"System.Mem.StableName.Map".'System.Mem.StableName.Map.Map' f@
-- is only permitted (without unsafe functions) when @f@'s parameter
-- has a representational role. This module relaxes that restriction
-- by using 'Typeable' to distinguish between values with the same
-- representation but different types.
module System.Mem.StableName.TypedMap
    ( Map
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

import GHC.Exts (Any)
import Prelude hiding (lookup, null, any)
import System.Mem.StableName
import System.Mem.StableName.TaggedSN
import qualified Data.HashMap.Lazy as M
import qualified Data.HashMap.Strict as MS
import Data.HashMap.Lazy (HashMap)
import Unsafe.Coerce (unsafeCoerce)
import Data.Typeable (Typeable)
import Data.Hashable (Hashable)

newtype Map f = Map { getMap :: HashMap TaggedSN (f Any) }
#if __GLASGOW_HASKELL__ >= 707
type role Map nominal
#endif

-- These instances are extremely restrictive, but they're the
-- best we can do!
instance Show (f Any) => Show (Map f) where
  showsPrec d (Map m) = showParen (d > 10) $
    showString "Map " . showsPrec 0 (M.toList m)

deriving instance Hashable (f Any) => Hashable (Map f)

deriving instance Eq (f Any) => Eq (Map f)

-- unsafe combinators
any :: f a -> f Any
any = unsafeCoerce

liftAny1 :: (f a -> f a) -> f Any -> f Any
liftAny1 f a = unsafeCoerce $ f (unsafeCoerce a)

liftAny2 :: (f a -> f a -> f a) -> f Any -> f Any -> f Any
liftAny2 f a = unsafeCoerce f a

empty :: Map f
empty = Map M.empty

null :: Map f -> Bool
null (Map m) = M.null m

singleton :: Typeable a => StableName a -> f a -> Map f
singleton k v = Map $ M.singleton (wrapTaggedSN k) (any v)

member :: Typeable a => StableName a -> Map f -> Bool
member k m = case lookup k m of
    Nothing -> False
    Just _ -> True

notMember :: Typeable a => StableName a -> Map f -> Bool
notMember k m = not $ member k m

insert :: Typeable a => StableName a -> f a -> Map f -> Map f
insert k v =
    Map .
    M.insert (wrapTaggedSN k) (any v) .
    getMap

-- | /O(log n)/. Insert with a function for combining the new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if the key does not exist
-- in the map. If the key does exist, the function will insert the pair
-- @(key, f new_value old_value)@
insertWith :: Typeable a => (f a -> f a -> f a) -> StableName a -> f a -> Map f -> Map f
insertWith f k v = Map . M.insertWith (liftAny2 f) (wrapTaggedSN k) (any v) . getMap

-- | Same as 'insertWith', but with the combining function applied strictly.
insertWith' :: Typeable a => (f a -> f a -> f a) -> StableName a -> f a -> Map f -> Map f
insertWith' f k v = Map . MS.insertWith (liftAny2 f) (wrapTaggedSN k) (any v) . getMap

adjust :: Typeable a => (f a -> f a) -> StableName a -> Map f -> Map f
adjust f k = Map . M.adjust (liftAny1 f) (wrapTaggedSN k) . getMap

adjust' :: Typeable a => (f a -> f a) -> StableName a -> Map f -> Map f
adjust' f k = Map . MS.adjust (liftAny1 f) (wrapTaggedSN k) . getMap

-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as a @('Just' value)@
-- or 'Nothing' if the key isn't in the map.
lookup :: Typeable a => StableName a -> Map f -> Maybe (f a)
lookup k (Map m) = do
  val <- M.lookup (wrapTaggedSN k) m
  return (unsafeCoerce val)

find :: Typeable a => StableName a -> Map f -> f a
find k m = case lookup k m of
    Nothing -> error "Map.find: element not in the map"
    Just x -> x

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns the default value @def@
-- when the key is not in the map.
findWithDefault :: Typeable a => f a -> StableName a -> Map f -> f a
findWithDefault dflt k m = maybe dflt id $ lookup k m
