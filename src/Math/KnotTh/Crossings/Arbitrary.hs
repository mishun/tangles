module Math.KnotTh.Crossings.Arbitrary
	( ArbitraryCrossing(..)
	, overCrossing
	, underCrossing
	, arbitraryCrossings
	, passOver
	, passUnder
	) where

import Control.DeepSeq
import Math.Algebra.Group.D4 (i, c, subGroupDS)
import Math.KnotTh.Knotted


data ArbitraryCrossing = ArbitraryCrossing deriving (Eq)


instance NFData ArbitraryCrossing


instance CrossingType ArbitraryCrossing where
	localCrossingSymmetry _ = subGroupDS

	possibleOrientations _ _ = arbitraryCrossings


instance Show ArbitraryCrossing where
	show _ = "-|-"


overCrossing :: CrossingState ArbitraryCrossing
overCrossing = makeCrossing ArbitraryCrossing i


underCrossing :: CrossingState ArbitraryCrossing
underCrossing = makeCrossing ArbitraryCrossing c


arbitraryCrossings :: [CrossingState ArbitraryCrossing]
arbitraryCrossings = [overCrossing, underCrossing]


passOver :: (Knotted k c d) => d ArbitraryCrossing -> Bool
passOver = even . crossingLegIdByDart


passUnder :: (Knotted k c d) => d ArbitraryCrossing -> Bool
passUnder = odd . crossingLegIdByDart
