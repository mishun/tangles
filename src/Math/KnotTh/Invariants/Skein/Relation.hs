module Math.KnotTh.Invariants.Skein.Relation
	( StateSummand(..)
	, StateSum
	, normalizeStateSum
	, SkeinKnotted(..)
	, InitialSum(..)
	, fromInitialSum
	, SkeinRelation(..)
	) where

import Data.List (foldl')
import qualified Data.Map as M
import Data.Array.Unboxed (UArray, listArray)
import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary
import qualified Math.KnotTh.Link.NonAlternating as L
import qualified Math.KnotTh.Tangle.NonAlternating as T


data StateSummand a = StateSummand !(UArray Int Int) a deriving (Show)


type StateSum a = [StateSummand a]


normalizeStateSum :: (Eq a, Num a) => StateSum a -> StateSum a
normalizeStateSum =
	map (\ (!k, !v) -> StateSummand k v) .
		filter ((/= 0) . snd) . M.toList .
			foldl' (\ !m (StateSummand !k !v) -> M.insertWith' (+) k v m) M.empty


class (Knotted k c d, Eq (d ArbitraryCrossing)) => SkeinKnotted k c d | k -> c, c -> d, d -> k where
	endpointPlace :: d ArbitraryCrossing -> Int
	endpointPlace = error "endpointPlace: must be no endpoints"


instance SkeinKnotted L.Link L.Crossing L.Dart


instance SkeinKnotted T.Tangle T.Crossing T.Dart where
	endpointPlace = T.legPlace


--    L+       L-        L0        L∞
--   \ /      \ /       \  /      \ /
--    /        \         ||        =
--   / \      / \       /  \      / \


data InitialSum a = InitialSum { ofLplus :: a, ofLzero :: a, ofLinfty :: a }


fromInitialSum :: (Eq a, Num a) => InitialSum a -> StateSum a
fromInitialSum x =
	filter (\ (StateSummand _ k) -> k /= 0) $
		[ StateSummand (listArray (0, 3) [3, 2, 1, 0]) (ofLzero x)
		, StateSummand (listArray (0, 3) [1, 0, 3, 2]) (ofLinfty x)
		, StateSummand (listArray (0, 3) [2, 3, 0, 1]) (ofLplus x)
		]


class (Eq a, Num a) => SkeinRelation r a | r -> a where
	initialLplus       :: r -> InitialSum a
	circleMultiple     :: r -> a
	finalNormalization :: (SkeinKnotted k c d) => r -> k ArbitraryCrossing -> a -> a

	finalNormalization _ _ = id
