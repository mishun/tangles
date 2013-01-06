module Math.KnotTh.Invariants.Skein.Relation
	( SkeinRelation(..)
	) where

import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Invariants.Skein.StateSum


--    L+       L-        L0        L∞
--   \ /      \ /       \  /      \ /
--    /        \         ||        =
--   / \      / \       /  \      / \
class (Ord a, Num a, Show a) => SkeinRelation r a | r -> a where
	initialLplus       :: r -> InitialSum a
	circleFactor       :: r -> a
	twistPFactor       :: r -> a
	twistNFactor       :: r -> a
	finalNormalization :: (Knotted k c d, Eq (d ArbitraryCrossing)) => r -> k ArbitraryCrossing -> a -> a

	finalNormalization _ _ = id
