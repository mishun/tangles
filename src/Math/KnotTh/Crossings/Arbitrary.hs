module Math.KnotTh.Crossings.Arbitrary
	( ArbitraryCrossing(..)
	, overCrossing
	, underCrossing
	, arbitraryCrossings
	, passOver
	, passUnder
	, passOver'
	, passUnder'
	, isAltEdge
	, writhe
	, invertCrossings
	) where

import Control.DeepSeq
import Math.Algebra.Group.D4 ((<*>), i, c, subGroupDS, equivalenceClassId)
import Math.KnotTh.Knotted


data ArbitraryCrossing = ArbitraryCrossing deriving (Eq)


instance NFData ArbitraryCrossing


instance CrossingType ArbitraryCrossing where
	localCrossingSymmetry _ = subGroupDS

	possibleOrientations _ bound =
		case bound of
			Nothing                                       -> arbitraryCrossings
			Just g | equivalenceClassId subGroupDS g == 0 -> arbitraryCrossings
			Just _                                        -> overCrossingOnly


instance Show ArbitraryCrossing where
	show _ = "-|-"


overCrossing :: CrossingState ArbitraryCrossing
overCrossing = makeCrossing ArbitraryCrossing i


underCrossing :: CrossingState ArbitraryCrossing
underCrossing = makeCrossing ArbitraryCrossing c


arbitraryCrossings :: [CrossingState ArbitraryCrossing]
arbitraryCrossings = [overCrossing, underCrossing]


overCrossingOnly :: [CrossingState ArbitraryCrossing]
overCrossingOnly = [overCrossing]


passOver :: (Knotted k c d) => d ArbitraryCrossing -> Bool
passOver = even . crossingLegIdByDart


passUnder :: (Knotted k c d) => d ArbitraryCrossing -> Bool
passUnder = odd . crossingLegIdByDart


passOver' :: CrossingState ArbitraryCrossing -> Int -> Bool
passOver' cr p = even $ crossingLegIdByDartId cr p


passUnder' :: CrossingState ArbitraryCrossing -> Int -> Bool
passUnder' cr p = odd $ crossingLegIdByDartId cr p


isAltEdge :: (Knotted k c d) => d ArbitraryCrossing -> Bool
isAltEdge d = passOver d == passUnder (opposite d)


writhe :: (Eq (d ArbitraryCrossing), Eq (c ArbitraryCrossing), Knotted t c d) => d ArbitraryCrossing -> d ArbitraryCrossing -> Int
writhe a b
	| incidentCrossing a /= incidentCrossing b  = error "writhe: darts must be incident to same crossing"
	| a == nextCCW b                            = d
	| a == nextCW b                             = -d
	| otherwise                                 = error "writhe: bad path"
	where
		d = if passOver b then 1 else -1


invertCrossings :: (Knotted k c d) => k ArbitraryCrossing -> k ArbitraryCrossing
invertCrossings = mapCrossingStates (alterCrossingOrientation (c <*>))
