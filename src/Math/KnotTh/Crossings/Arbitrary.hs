module Math.KnotTh.Crossings.Arbitrary
	( module Math.KnotTh.Crossings
	, ArbitraryCrossing(..)
	, overCrossing
	, underCrossing
	, arbitraryCrossings
	, passOverById
	, passUnderById
	) where

import Control.DeepSeq
import Math.Algebra.Group.D4 (i, c, subGroupDS)
import Math.KnotTh.Crossings


data ArbitraryCrossing = ArbitraryCrossing deriving (Eq)


instance NFData ArbitraryCrossing


instance CrossingType ArbitraryCrossing where
	localCrossingSymmetry _ = subGroupDS

	possibleOrientations _ _ = arbitraryCrossings


instance Show ArbitraryCrossing where
	show _ = "-|-"


overCrossing :: CrossingState ArbitraryCrossing
overCrossing = crossing ArbitraryCrossing i


underCrossing :: CrossingState ArbitraryCrossing
underCrossing = crossing ArbitraryCrossing c


arbitraryCrossings :: [CrossingState ArbitraryCrossing]
arbitraryCrossings = [overCrossing, underCrossing]


passOverById :: CrossingState ArbitraryCrossing -> Int -> Bool
passOverById cr = even . crossingLegByDart cr


passUnderById :: CrossingState ArbitraryCrossing -> Int -> Bool
passUnderById cr = not . passOverById cr
