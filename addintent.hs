{-# LANGUAGE DatatypeContexts #-}

import Data.Set (Set)
import qualified Data.Set as Set

-- X=(A,B), extent of X = A, intent of X = B
-- intent of A = B


-- this is like a type alias yo.
data (Eq a, Eq b, Ord a, Ord b) => Context a b =
    Context1 (Set a) (Set b) (Set (a, b))
    deriving (Show, Eq)

-- A' = { m in M | all g in A , gIm }
image :: (Ord a, Ord b) => Set (a, b) -> a -> Set b
image rel obj = Set.map snd $ Set.filter (\(x,y) -> x == obj) rel

preimage :: (Ord a, Ord b) => Set (a, b) -> b -> Set a
preimage rel attr = Set.map fst $ Set.filter (\(x,y) -> y == attr) rel

getIntent :: (Ord a, Ord b) => Context a b -> Set a -> Set b
getIntent (Context1 g m rel) objs = Set.fold Set.intersection m $ Set.map (image rel) objs

getExtent :: (Ord a, Ord b) => Context a b -> Set b -> Set a
getExtent (Context1 g m rel) attr = Set.fold Set.intersection g $ Set.map (preimage rel) attr


--let g = Set.fromList ["a","b"]
--let rel = Set.fromList [("a",1)]
--let m = Set.fromList [1,2]
--let ctx = Context1 g m rel
