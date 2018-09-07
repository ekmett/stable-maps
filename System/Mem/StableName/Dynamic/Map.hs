{-# LANGUAGE CPP #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE Unsafe #-}
#endif
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StandaloneDeriving #-}
module System.Mem.StableName.Dynamic.Map
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
    , fromList
    , lookup
    , find
    , findWithDefault
    ) where

import Prelude hiding (lookup, null)
import System.Mem.StableName.Dynamic
import qualified Data.HashMap.Lazy as M
import qualified Data.HashMap.Strict as MS
import Data.HashMap.Lazy (HashMap)
import qualified Data.Foldable as F
import qualified Data.Traversable as T
import Data.Hashable (Hashable)
#if MIN_VERSION_base(4,9,0)
import Data.Functor.Classes (Eq1, Show1 (..))
#endif

newtype Map a = Map { getMap :: HashMap DynamicStableName a }
  deriving (Eq, Hashable, Functor, F.Foldable, T.Traversable)

instance Show a => Show (Map a) where
  showsPrec d (Map m) = showsPrec d m

#if MIN_VERSION_base(4,9,0)
deriving instance Eq1 Map

instance Show1 Map where
  liftShowsPrec se sl p (Map m) = liftShowsPrec se sl p m
#endif

empty :: Map a
empty = Map M.empty

null :: Map a -> Bool
null (Map m) = M.null m

singleton :: DynamicStableName -> a -> Map a
singleton k v = Map $ M.singleton k v

member :: DynamicStableName -> Map a -> Bool
member k m = case lookup k m of
    Nothing -> False
    Just _ -> True

notMember :: DynamicStableName -> Map a -> Bool
notMember k m = not $ member k m 

insert :: DynamicStableName -> a -> Map a -> Map a
insert k v = Map . M.insert k v . getMap

fromList :: [(DynamicStableName, a)] -> Map a
fromList = Map . M.fromList
    
-- | /O(log n)/. Insert with a function for combining the new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if the key does not exist
-- in the map. If the key does exist, the function will insert the pair
-- @(key, f new_value old_value)@
insertWith :: (a -> a -> a) -> DynamicStableName -> a -> Map a -> Map a
insertWith f k v = Map . M.insertWith f k v . getMap 

-- | Same as 'insertWith', but with the combining function applied strictly.
insertWith' :: (a -> a -> a) -> DynamicStableName -> a -> Map a -> Map a
insertWith' f k v = Map . MS.insertWith f k v . getMap 

adjust :: (a -> a) -> DynamicStableName -> Map a -> Map a
adjust f k = Map . M.adjust f k . getMap

adjust' :: (a -> a) -> DynamicStableName -> Map a -> Map a
adjust' f k = Map . MS.adjust f k . getMap

-- | /O(log n)/. Lookup the value at a key in the map.
-- 
-- The function will return the corresponding value as a @('Just' value)@
-- or 'Nothing' if the key isn't in the map.
lookup :: DynamicStableName -> Map v -> Maybe v
lookup k (Map m) = M.lookup k m

find :: DynamicStableName -> Map v -> v
find k m = case lookup k m of
    Nothing -> error "Map.find: element not in the map"
    Just x -> x 

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns the default value @def@
-- when the key is not in the map.
findWithDefault :: v -> DynamicStableName -> Map v -> v
findWithDefault dflt k m = maybe dflt id $ lookup k m 
