{-# LANGUAGE DatatypeContexts #-}

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
-- X=(A,B), extent of X = A, intent of X = B
-- intent of A = B


-- this is like a type alias yo.
data (Eq a, Eq b, Ord a, Ord b) => Context a b =
    Context1 (Set a) (Set b) (Set (a, b))
    deriving (Show, Eq)

data (Eq a, Eq b, Ord a, Ord b) => Concept a b =
    Concept (Set a) (Set b)
    deriving (Show, Eq)

data Node a b = Node (Concept a b) (Set (Concept a b)) (Set (Concept a b))
data Lattice a b =
    Lattice Map (Concept a b) (Node a b)

makeLattice :: Set (a,b) -> Lattice Map (Concept a b) (Node a b)
makeLattice s = Set.map undefined s

-- A' = { m in M | all g in A , gIm }
image :: (Ord a, Ord b) => Set (a, b) -> a -> Set b
image rel obj = Set.map snd $ Set.filter (\(x,y) -> x == obj) rel

preimage :: (Ord a, Ord b) => Set (a, b) -> b -> Set a
preimage rel attr = Set.map fst $ Set.filter (\(x,y) -> y == attr) rel

getIntent :: (Ord a, Ord b) => Context a b -> Set a -> Set b
getIntent (Context1 g m rel) objs = Set.fold Set.intersection m $ Set.map (image rel) objs

getExtent :: (Ord a, Ord b) => Context a b -> Set b -> Set a
getExtent (Context1 g m rel) attr = Set.fold Set.intersection g $ Set.map (preimage rel) attr


-- intent is a set of attributes
-- (obs, attr) generator concept is a generator concept
addIntent intent (Concept objs attr) lattice = undefined

getMaximalConcept intent c lattice@(Set n) =



Function GetMaximalConcept(intent, GeneratorConcept, L)
  parentIsMaximal := true
  While parentIsMaximal
    parentIsMaximal := false
    Parents := GetParents(GeneratorConcept, L )
    For each Parent in Parents If intent ⊆ Parent.Intent
      GeneratorConcept := Parent parentIsMaximal := true Exit For
    End If End For
  Return GeneratorConcept





--let g = Set.fromList ["a","b"]
--let rel = Set.fromList [("a",1)]
--let m = Set.fromList [1,2]
--let ctx = Context1 g m rel
