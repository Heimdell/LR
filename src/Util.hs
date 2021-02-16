
-- | A /svalka/.
module Util (module Util, module Debug.Trace) where

import Set qualified as Set
import Set (Set)

import Map qualified as Map
import Map (Map)

import Pretty hiding (isEmpty)

import Debug.Trace

-- | Calculate a transitive closure of a function over a monoid
--   with subtraction.
--
close :: (Eq t, Subtractable t, Pretty t) => (t -> t) -> t -> t
close f t = go t t  -- accumulator and a working pool
  where
    go acc s = do
      let s' = f s              -- apply f to working pool
      let s'' = s' `minus` acc  -- remove elemens aready in accumulator
      if isEmpty s''            -- no elements?
      then acc                  --   return accumulator
      else go (acc <> s'') s''  --   add new ones to accum and use them as pool

-- | Trace a function call.
--
traceFun :: (Pretty t, Pretty s) => String -> (s -> t) -> s -> t
traceFun msg f s =
  let t = f s
  in traceShow (msg, pretty s, "==>" :: String, pretty t) t

-- | Extension of `Monoid` interface, add subtraction and comparison with zero.
--
class Monoid t => Subtractable t where
  minus   :: t -> t -> t  -- ^ subtract
  isEmpty :: t -> Bool    -- ^ compare with zero

instance Ord t => Subtractable (Set t) where
  minus   = Set.diff
  isEmpty = Set.null

instance (Ord k, Eq v, Monoid v, Subtractable v) => Subtractable (Map k v) where
  minus s t = foldTheMap \k v -> do
    let v' = v `minus` (t? k)
    if Util.isEmpty v'
    then id
    else Map.insert k v'
    where
      foldTheMap f = Map.foldrWithKey f mempty s

  isEmpty = Map.null

-- | An interface for accessing elements.
--
class Get box ix res | box -> ix res where
  (?)  :: box -> ix -> res  -- ^ read an element
  keys :: box -> Set ix     -- ^ get all keys

infixl 8 ?

instance (Ord k) => Get (Set k) k Bool where
  (?)  = flip Set.member
  keys = id

instance (Ord k, Monoid v) => Get (Map k v) k v where
  (?)  = (maybe mempty id .) . flip Map.lookup
  keys = Map.keySet

-- | `mempty` is no value, apply a function otherwise.
--
possible :: Monoid b => (a -> b) -> Maybe a -> b
possible = maybe mempty

-- | Memoize a function into map, given set of all inputs.
--
materialize :: Ord a => (a -> b) -> Set a -> Map a b
materialize f (Set.toList -> keys') = Map.fromList $ zip keys' (map f keys')

-- | Make a normal function out of memoized.
--
dematerialise :: (Ord a, Monoid b) => Map a b -> a -> b
dematerialise = (?)

-- | Memoize a function, given set of all inputs.
--
memoise :: (Ord a, Monoid b) => Set a -> (a -> b) -> (a -> b)
memoise args f = dematerialise (materialize f args)
