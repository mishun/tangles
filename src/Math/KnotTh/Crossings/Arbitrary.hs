module Math.KnotTh.Crossings.Arbitrary
	( ArbitraryCrossing(..)
	, ArbitraryCrossingState
	, overCrossing
	, underCrossing
	, arbitraryCrossings
	, overCrossingOnly
	, isOverCrossing
	, isUnderCrossing
	, passOver
	, passUnder
	, passOver'
	, passUnder'
	, alternatingDefect
	, isAlternating
	, writhe
	, invertCrossings
	) where

import Control.DeepSeq
import Math.Algebra.Group.D4 (i, c, subGroupDS, equivalenceClassId)
import Math.KnotTh.Knotted


data ArbitraryCrossing = ArbitraryCrossing deriving (Eq)


instance NFData ArbitraryCrossing


instance CrossingType ArbitraryCrossing where
	localCrossingSymmetry _ = subGroupDS

	possibleOrientations _ bound =
		case bound of
			Nothing                                       -> arbitraryCrossings
			Just g | equivalenceClassId subGroupDS g == 0 -> arbitraryCrossings
			       | otherwise                            -> overCrossingOnly


instance Show ArbitraryCrossing where
	show _ = "-|-"


type ArbitraryCrossingState = CrossingState ArbitraryCrossing


overCrossing :: ArbitraryCrossingState
overCrossing = makeCrossing ArbitraryCrossing i


underCrossing :: ArbitraryCrossingState
underCrossing = makeCrossing ArbitraryCrossing c


arbitraryCrossings :: [ArbitraryCrossingState]
arbitraryCrossings = [overCrossing, underCrossing]


overCrossingOnly :: [ArbitraryCrossingState]
overCrossingOnly = [overCrossing]


isOverCrossing :: ArbitraryCrossingState -> Bool
isOverCrossing s = passOver' s 0


isUnderCrossing :: ArbitraryCrossingState -> Bool
isUnderCrossing s = passUnder' s 0


passOver :: (Knotted k c d) => d ArbitraryCrossing -> Bool
passOver = even . crossingLegIdByDart


passUnder :: (Knotted k c d) => d ArbitraryCrossing -> Bool
passUnder = odd . crossingLegIdByDart


passOver' :: ArbitraryCrossingState -> Int -> Bool
passOver' cr p = even $ crossingLegIdByDartId cr p


passUnder' :: ArbitraryCrossingState -> Int -> Bool
passUnder' cr p = odd $ crossingLegIdByDartId cr p


alternatingDefect :: (Knotted k c d) => k ArbitraryCrossing -> Int
alternatingDefect =
	let defect (!a, !b)
		| isEndpoint a || isEndpoint b  = 0
		| passOver a == passOver b      = 1
		| otherwise                     = 0
	in sum . map defect . allEdges


isAlternating :: (Knotted k c d) => k ArbitraryCrossing -> Bool
isAlternating = (== 0) . alternatingDefect


writhe :: (Eq (d ArbitraryCrossing), Eq (c ArbitraryCrossing), Knotted t c d) => d ArbitraryCrossing -> d ArbitraryCrossing -> Int
writhe a b
	| incidentCrossing a /= incidentCrossing b  = error "writhe: darts must be incident to same crossing"
	| a == nextCCW b                            = d
	| a == nextCW b                             = -d
	| otherwise                                 = error "writhe: bad path"
	where
		d | passOver b  = 1
		  | otherwise   = -1


invertCrossings :: (Knotted k c d) => k ArbitraryCrossing -> k ArbitraryCrossing
invertCrossings = mapCrossings $ \ s ->
	if isOverCrossing s
		then underCrossing
		else overCrossing
