module Math.KnotTh.Invariants.Skein.Relation
	( module Math.KnotTh.Knotted
	, module Math.KnotTh.Crossings.Arbitrary
	, module Math.KnotTh.Invariants.Skein.StateSum
	, SkeinKnotted(..)
	, SkeinRelation(..)
	) where

import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary
import qualified Math.KnotTh.Link.NonAlternating as L
import qualified Math.KnotTh.Tangle.NonAlternating as T
import Math.KnotTh.Invariants.Skein.StateSum


--    L+       L-        L0        L∞
--   \ /      \ /       \  /      \ /
--    /        \         ||        =
--   / \      / \       /  \      / \

class (Eq a, Num a, Show a) => SkeinRelation r a | r -> a where
	initialLplus       :: r -> InitialSum a
	circleFactor       :: r -> a
	twistPFactor       :: r -> a
	twistNFactor       :: r -> a
	finalNormalization :: (SkeinKnotted k c d) => r -> k ArbitraryCrossing -> a -> a

	finalNormalization _ _ = id


class (Knotted k c d, Eq (d ArbitraryCrossing)) => SkeinKnotted k c d | k -> c, c -> d, d -> k where
	endpointPlace :: d ArbitraryCrossing -> Int
	endpointPlace = error "endpointPlace: must be no endpoints"


instance SkeinKnotted L.Link L.Crossing L.Dart


instance SkeinKnotted T.Tangle T.Crossing T.Dart where
	endpointPlace = T.legPlace
