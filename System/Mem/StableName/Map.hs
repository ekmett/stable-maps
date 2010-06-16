module System.Mem.StableName.Map 
    ( empty
    , null
    , singleton
    , member
    , notMember
    , insert
    , insertWith
    , insertWith'
    , adjust
    , lookup
    , find
    , findWithDefault
    ) where

import System.Mem.StableName
import Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Unsafe.Coerce (unsafeCoerce)

newtype Map f = Map { getMap :: IntMap [(DynamicStableName, f Any)] } 

-- unsafe combinators
any :: f a -> f Any
any = unsafeCoerce

some :: f Any -> f a
some = unsafeCoerce

liftAny1 :: (f a -> f a) -> f Any -> f Any 
liftAny1 f a = unsafeCoerce $ f (unsafeCoerce a)

liftAny2 :: (f a -> f a -> f a) -> f Any -> f Any -> f Any
liftAny2 f a b = unsafeCoerce $ f (unsafeCoerce a) (unsafeCoerce a)

empty :: Map f
empty = Map IntMap.empty

null :: Map f -> Bool
null (Map m) = null m

singleton :: StableName a -> f a -> Map f
singleton k v = Map $ IntMap.singleton (hashDynamicStableName dk) [(dk, any v)]
    where
        dk = wrapStableName k

member :: StableName a -> Map f -> Bool
member k m = case lookup k m of
    Nothing -> False
    Just _ -> True

notMember :: DynamicStableName -> Map a -> Bool
notMember k m = not $ member k m 

insert :: DynamicStableName -> a -> Map a -> Map a
insert k v = 
    Map . 
    IntMap.insertWith (++) (hashDynamicStableName dk) [(dk,any v)] . 
    getMap
    where 
        dk = wrapStableName k

-- | /O(log n)/. Insert with a function for combining the new value and old value.
-- @'insertWith' f key value mp@
-- will insert the pair (key, value) into @mp@ if the key does not exist
-- in the map. If the key does exist, the function will insert the pair
-- @(key, f new_value old_value)@
insertWith :: (f a -> f a -> f a) -> StableName a -> f a -> Map f -> Map f
insertWith f k v = Map . IntMap.insertWith go (hashDynamicStableName dk) [(dk,any v)] . getMap 
    where 
        dk = wrapStableName k
        go _ ((k',v'):kvs) 
            | dk == k' = (k', liftAny2 f v v') : kvs
            | otherwise = (k',v') : go kvs
        go _ [] = [(dk, any v)]

-- | Same as 'insertWith', but with the combining function applied strictly.
insertWith' :: (f a -> f a -> f a) -> StableName a -> a -> Map f -> Map f
insertWith' f k v = Map . IntMap.insertWith go (hashDynamicStableName dk) [(dk, any v)] . getMap 
    where 
        dk = wrapStableName k
        go _ ((k',v'):kvs) 
            | dk == k' = let v'' = liftAny2 f v v' in v'' `seq` (k', v'') : kvs
            | otherwise = (k', v') : go undefined kvs
        go _ [] = [(dk, any v)]

adjust :: (f a -> f a) -> StableName a -> Map f -> Map f
adjust f k = Map . IntMap.adjust go (hashDynamicStableName dk) . getMap
    where
        dk = wrapStableName k
        go ((k',v):kvs)
            | dk == k' = (k', liftAny1 f v) : kvs
            | otherwise = (k', v') : go kvs
        go [] = []

-- | /O(log n)/. Lookup the value at a key in the map.
-- 
-- The function will return the corresponding value as a @('Just' value)@
-- or 'Nothing' if the key isn't in the map.
lookup :: StableName a -> Map f -> Maybe (f a)
lookup k (Map m) = do
    pairs <- IntMap.lookup (hashDynamicStableName dk) m
    unsafeCoerce $ Prelude.lookup dk pairs
    where
        dk = wrapStableName k 

find :: StableName a -> Map f -> f a 
find = case lookup k m of
    Nothing -> error "Map.find: element not in the map"
    Just x -> x 

-- | /O(log n)/. The expression @('findWithDefault' def k map)@ returns
-- the value at key @k@ or returns the default value @def@
-- when the key is not in the map.
findWithDefault :: f a -> StableName a -> Map f -> f a
findWithDefault dflt m = maybe dflt id $ lookup k m 
