{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Unsafe #-}
#endif
#if __GLASGOW_HASKELL__ >= 708
{-# LANGUAGE RoleAnnotations #-}
#endif
{-# LANGUAGE TypeFamilies #-}
#if __GLASGOW_HASKELL__ >= 805
{-# LANGUAGE QuantifiedConstraints, MonoLocalBinds #-}
#endif
{-# LANGUAGE FlexibleInstances, FlexibleContexts, UndecidableInstances #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE RankNTypes #-}

module System.Mem.StableName.Map.Internal
    ( Map (..)
    , Representational
#if (__GLASGOW_HASKELL__ >= 708) && (__GLASGOW_HASKELL__ < 805)
    -- This prevents a stupid warning, but we don't actually
    -- expose it outside the package.
    , Skolem2 (..)
#endif
    , empty
    , null
    , singleton
    , member
    , notMember
    , insert
    , insertWith
    , insertWith'
    , unsafeInsertWith
    , unsafeInsertWith'
    , adjust
    , adjust'
    , unsafeAdjust
    , unsafeAdjust'
    , hmap
    , hfoldMap
    , htraverse
    , hmap'
    , htraverse'
    , lookup
    , unsafeLookup
    , find
    , unsafeFind
    , findWithDefault
    , unsafeFindWithDefault
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
import qualified Data.Foldable as F
import qualified Data.Traversable as T

newtype Map f = Map { getMap :: HashMap DynamicStableName (f Any) }
#if __GLASGOW_HASKELL__ >= 708
type role Map nominal
#endif

#if __GLASGOW_HASKELL__ >= 805
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

empty :: Map f
empty = Map M.empty

null :: Map f -> Bool
null (Map m) = M.null m

singleton :: StableName a -> f a -> Map f
singleton k v = Map $ M.singleton (wrapStableName k) (any v)

member :: StableName a -> Map f -> Bool
member k m = case unsafeLookup k m of
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
insertWith
  :: Representational f
  => (f a -> f a -> f a) -> StableName a -> f a -> Map f -> Map f
insertWith = unsafeInsertWith

unsafeInsertWith
  :: (f a -> f a -> f a) -> StableName a -> f a -> Map f -> Map f
unsafeInsertWith f k v =
  Map . M.insertWith (liftAny2 f) (wrapStableName k) (any v) . getMap

-- | Same as 'insertWith', but with the combining function applied strictly.
insertWith'
  :: Representational f
  => (f a -> f a -> f a) -> StableName a -> f a -> Map f -> Map f
insertWith' = unsafeInsertWith'

unsafeInsertWith' :: (f a -> f a -> f a) -> StableName a -> f a -> Map f -> Map f
unsafeInsertWith' f k v =
  Map . MS.insertWith (liftAny2 f) (wrapStableName k) (any v) . getMap

adjust
  :: Representational f
  => (f a -> f a) -> StableName a -> Map f -> Map f
adjust = unsafeAdjust

unsafeAdjust :: (f a -> f a) -> StableName a -> Map f -> Map f
unsafeAdjust f k = Map . M.adjust (liftAny1 f) (wrapStableName k) . getMap

adjust'
  :: Representational f
  => (f a -> f a) -> StableName a -> Map f -> Map f
adjust' = unsafeAdjust'

unsafeAdjust' :: (f a -> f a) -> StableName a -> Map f -> Map f
unsafeAdjust' f k = Map . MS.adjust (liftAny1 f) (wrapStableName k) . getMap

hmap
  :: (forall a. f a -> g a)
  -> Map f -> Map g
hmap f (Map m) = Map (fmap f m)

hmap'
  :: (forall a. f a -> g a)
  -> Map f -> Map g
hmap' f (Map m) = Map (MS.map f m)

hfoldMap
  :: Monoid m
  => (forall a. f a -> m)
  -> Map f -> m
hfoldMap f (Map m) = F.foldMap f m

htraverse
  :: Applicative m
  => (forall a. f a -> m (g a))
  -> Map f -> m (Map g)
htraverse f (Map m) = Map <$> T.traverse f m

htraverse'
  :: Applicative m
  => (forall a. f a -> m (g a))
  -> Map f -> m (Map g)
htraverse' f (Map m) = Map <$> MS.traverseWithKey (\_ v -> f v) m

-- | /O(log n)/. Lookup the value at a key in the map.
--
-- The function will return the corresponding value as a @('Just' value)@
-- or 'Nothing' if the key isn't in the map.
lookup
  :: Representational f
  => StableName a -> Map f -> Maybe (f a)
lookup = unsafeLookup

unsafeLookup :: StableName a -> Map f -> Maybe (f a)
unsafeLookup k (Map m) = unsafeCoerce (M.lookup (wrapStableName k) m)

find :: Representational f => StableName a -> Map f -> f a
find = unsafeFind

unsafeFind :: StableName a -> Map f -> f a
unsafeFind k m = case unsafeLookup k m of
    Nothing -> error "Map.find: element not in the map"
    Just x -> x

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns the default value @def@
-- when the key is not in the map.
findWithDefault :: Representational f => f a -> StableName a -> Map f -> f a
findWithDefault = unsafeFindWithDefault

unsafeFindWithDefault :: f a -> StableName a -> Map f -> f a
unsafeFindWithDefault dflt k m = maybe dflt id $ unsafeLookup k m
