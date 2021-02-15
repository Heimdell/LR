
module Util (module Util, module Debug.Trace) where

import Set qualified as Set
import Set (Set)

import Map qualified as Map
import Map (Map)

import Pretty

import Debug.Trace

close :: (Eq t, Subtractable t, Monoid t, Pretty t) => (t -> t) -> t -> t
close f = go mempty
  where
    go acc s = s <> do
      let s' = f s
      let s'' = s' `minus` acc
      if s'' == mempty
      then acc
      else go (acc <> s'') s''

traceFun :: (Pretty t, Pretty s) => String -> (s -> t) -> s -> t
traceFun msg f s =
  let t = f s
  in traceShow (msg, pretty s, "==>" :: String, pretty t) t

class Subtractable t where
  minus   :: t -> t -> t
  isEmpty :: t -> Bool

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
      foldTheMap f = Map.foldrWithKey f Map.empty s

  isEmpty = Map.null

delta :: (Ord t, Pretty t) => (Set t -> Set t) -> Set t -> Set t
delta f s = let s' = f s in traceShow ("delta" :: String, Set.diff s s') s'

class Get box ix res | box -> ix res where
  (?)  :: box -> ix -> res
  keys :: box -> Set ix

infixl 8 ?

instance (Ord k) => Get (Set k) k Bool where
  (?)  = flip Set.member
  keys = id

instance (Ord k, Monoid v) => Get (Map k v) k v where
  (?)  = (maybe mempty id .) . flip Map.lookup
  keys = Map.keySet

possible :: Monoid b => (a -> b) -> Maybe a -> b
possible = maybe mempty

materialize :: Ord a => (a -> b) -> Set a -> Map a b
materialize f (Set.toList -> keys') = Map.fromList $ zip keys' (map f keys')

dematerialise :: (Ord a, Monoid b) => Map a b -> a -> b
dematerialise = (?)

memoise :: (Ord a, Monoid b) => Set a -> (a -> b) -> (a -> b)
memoise args f = dematerialise (materialize f args)
