module Math.KnotTh.Knotted.Util
	( crossingTypeInside
	, isCrossingOrientationInvertedInside
	, crossingLegIdByDart
	, dartByCrossingLegId
	, makeCrossing'
	, nextDir
	, continuation
	, begin
	, adjacentCrossing
	, incidentDarts
	, nthAdjacentDart
	, adjacentDarts
	, allCrossings
	, allDarts
	) where

import Math.Algebra.RotationDirection
import qualified Math.Algebra.Group.D4 as D4
import Math.KnotTh.Knotted.Knotted


{-# INLINE crossingTypeInside #-}
crossingTypeInside :: (CrossingType ct, Knotted k c d) => c ct -> ct
crossingTypeInside = crossingType . crossingState


{-# INLINE isCrossingOrientationInvertedInside #-}
isCrossingOrientationInvertedInside :: (CrossingType ct, Knotted k c d) => c ct -> Bool
isCrossingOrientationInvertedInside = isCrossingOrientationInverted . crossingState


{-# INLINE crossingLegIdByDart #-}
crossingLegIdByDart :: (CrossingType ct, Knotted t c d) => d ct -> Int
crossingLegIdByDart d = crossingLegIdByDartId (crossingState $ incidentCrossing d) (dartPlace d)


{-# INLINE dartByCrossingLegId #-}
dartByCrossingLegId :: (CrossingType ct, Knotted k c d) => c ct -> Int -> d ct
dartByCrossingLegId c = nthIncidentDart c . dartIdByCrossingLegId (crossingState c)


makeCrossing' :: (CrossingType ct) => ct -> CrossingState ct
makeCrossing' ct = makeCrossing ct D4.i



{-# INLINE nextDir #-}
nextDir :: (Knotted k c d) => RotationDirection -> d ct -> d ct
nextDir dir
	| isClockwise dir  = nextCW
	| otherwise        = nextCCW


{-# INLINE continuation #-}
continuation :: (Knotted k c d) => d ct -> d ct
continuation = nextCCW . nextCCW


{-# INLINE begin #-}
begin :: (Knotted k c d) => d ct -> (c ct, Int)
begin d =
	let	c = incidentCrossing d
		p = dartPlace d
	in c `seq` p `seq` (c, p)


{-# INLINE adjacentCrossing #-}
adjacentCrossing :: (Knotted k c d) => d ct -> c ct
adjacentCrossing = incidentCrossing . opposite


{-# INLINE incidentDarts #-}
incidentDarts :: (Knotted k c d) => c ct -> [d ct]
incidentDarts c = map (nthIncidentDart c) [0 .. 3]


{-# INLINE nthAdjacentDart #-}
nthAdjacentDart :: (Knotted k c d) => c ct -> Int -> d ct
nthAdjacentDart c = opposite . nthIncidentDart c


{-# INLINE adjacentDarts #-}
adjacentDarts :: (Knotted k c d) => c ct -> [d ct]
adjacentDarts c = map (nthAdjacentDart c) [0 .. 3]


{-# INLINE allCrossings #-}
allCrossings :: (Knotted k c d) => k ct -> [c ct]
allCrossings knot = map (nthCrossing knot) [1 .. numberOfCrossings knot]


{-# INLINE allDarts #-}
allDarts :: (Knotted k c d) => k ct -> [d ct]
allDarts = concatMap incidentDarts . allCrossings
