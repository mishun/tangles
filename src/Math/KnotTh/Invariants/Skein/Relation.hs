module Math.KnotTh.Invariants.Skein.Relation
	( StateSummand(..)
	, StateSum
	, SkeinKnotted(..)
	, SkeinRelation(..)
	) where

import Data.Array.Unboxed (UArray)
import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary
import qualified Math.KnotTh.Link.NonAlternating as L
import qualified Math.KnotTh.Tangle.NonAlternating as T


data StateSummand a = StateSummand !(UArray Int Int) a


type StateSum a = [StateSummand a]


class (Knotted k c d) => SkeinKnotted k c d | k -> c, c -> d, d -> k where
	endpointPlace :: d ArbitraryCrossing -> Int
	endpointPlace = error "endpointPlace: must be no endpoints"


instance SkeinKnotted L.Link L.Crossing L.Dart


instance SkeinKnotted T.Tangle T.Crossing T.Dart where
	endpointPlace = T.legPlace


--    L+       L-        L0        L∞
--   \ /      \ /       \  /      \ /
--    /        \         ||        =
--   / \      / \       /  \      / \

class (Num a) => SkeinRelation r a | r -> a where
	initialLplus :: r -> StateSum a
