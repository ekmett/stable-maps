{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Unsafe #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE RoleAnnotations #-}
#endif
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints, MonoLocalBinds #-}
#endif
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

module System.Mem.StableName.Map.Internal
    ( Map (..)
    , Representational
#if (__GLASGOW_HASKELL__ >= 708) && (__GLASGOW_HASKELL__ < 806)
    -- This prevents a stupid warning, but we don't actually
    -- expose it outside the package.
    , Skolem2 (..)
#endif
    , empty
    , unsafeEmpty
    , null
    , singleton
    , unsafeSingleton
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
import System.Mem.StableName.Dynamic
import qualified Data.HashMap.Lazy as M
import qualified Data.HashMap.Strict as MS
import Data.HashMap.Lazy (HashMap)
import Unsafe.Coerce (unsafeCoerce)
#if __GLASGOW_HASKELL__ >= 708
import Data.Coerce
#endif

newtype Map f = Map { getMap :: HashMap DynamicStableName (f Any) }
#if __GLASGOW_HASKELL__ >= 708
type role Map nominal
#endif

#if __GLASGOW_HASKELL__ >= 806
class (forall a b. Coercible a b => Coercible (f a) (f b))
  => Representational f
instance (forall a b. Coercible a b => Coercible (f a) (f b))
  => Representational f
#elif __GLASGOW_HASKELL__ >= 708
-- Is this safe? Could be....
data Skolem1
newtype Skolem2 = Skolem2 Skolem1

class Representational (f :: * -> *)
instance Coercible (f Skolem1) (f Skolem2) => Representational f
#else
-- Definitely not safe.
class Represesentational (f :: * -> *)
instance Representational f
#endif

-- unsafe combinators
any :: f a -> f Any
any = unsafeCoerce

liftAny1 :: (f a -> f a) -> f Any -> f Any
liftAny1 f a = unsafeCoerce f a

liftAny2 :: (f a -> f a -> f a) -> f Any -> f Any -> f Any
liftAny2 f a b = unsafeCoerce f a b

empty :: Representational f => Map f
empty = unsafeEmpty

unsafeEmpty :: Map f
unsafeEmpty = Map M.empty

null :: Map f -> Bool
null (Map m) = M.null m

singleton :: Representational f => StableName a -> f a -> Map f
singleton = unsafeSingleton

unsafeSingleton :: StableName a -> f a -> Map f
unsafeSingleton k v = Map $ M.singleton (wrapStableName k) (any v)

member :: StableName a -> Map f -> Bool
member k m = case lookup k m of
    Nothing -> False
    Just _ -> True

notMember :: StableName a -> Map f -> Bool
notMember k m = not $ member k m

insert :: StableName a -> f a -> Map f -> Map f
insert k v =
    Map .
    M.insert (wrapStableName k) (any v) .
    getMap

-- | /O(log n)/. Insert with a function for combining the new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if the key does not exist
-- in the map. If the key does exist, the function will insert the pair
-- @(key, f new_value old_value)@
insertWith :: (f a -> f a -> f a) -> StableName a -> f a -> Map f -> Map f
insertWith f k v =
  Map . M.insertWith (liftAny2 f) (wrapStableName k) (any v) . getMap

-- | Same as 'insertWith', but with the combining function applied strictly.
insertWith' :: (f a -> f a -> f a) -> StableName a -> f a -> Map f -> Map f
insertWith' f k v =
  Map . MS.insertWith (liftAny2 f) (wrapStableName k) (any v) . getMap

adjust :: (f a -> f a) -> StableName a -> Map f -> Map f
adjust f k = Map . M.adjust (liftAny1 f) (wrapStableName k) . getMap

adjust' :: (f a -> f a) -> StableName a -> Map f -> Map f
adjust' f k = Map . MS.adjust (liftAny1 f) (wrapStableName k) . getMap

-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as a @('Just' value)@
-- or 'Nothing' if the key isn't in the map.
lookup :: StableName a -> Map f -> Maybe (f a)
lookup k (Map m) = unsafeCoerce (M.lookup (wrapStableName k) m)

find :: StableName a -> Map f -> f a
find k m = case lookup k m of
    Nothing -> error "Map.find: element not in the map"
    Just x -> x

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns the default value @def@
-- when the key is not in the map.
findWithDefault :: f a -> StableName a -> Map f -> f a
findWithDefault dflt k m = maybe dflt id $ lookup k m
