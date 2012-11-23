{-# LANGUAGE KindSignatures, FunctionalDependencies #-}
module Math.KnotTh.Knotted
	( module Math.KnotTh.Crossings
	, Knotted(..)
	, nextDir
	, continuation
	, begin
	, adjacentCrossing
	, incidentDarts
	, nthAdjacentDart
	, adjacentDarts
	, allCrossings
	, allDarts
	, forMAdjacentDarts
	, foldMAdjacentDarts
	, foldMAdjacentDartsFrom
	) where

import Data.Bits ((.&.))
import Math.Algebra.RotationDirection
import Math.KnotTh.Crossings


class Knotted (k :: * -> *) (c :: * -> *) (d :: * -> *) | k -> d, k -> c, c -> k, c -> d, d -> k, d -> c where
	numberOfCrossings :: k ct -> Int
	numberOfEdges     :: k ct -> Int
	nthCrossing       :: k ct -> Int -> c ct
	mapCrossingStates :: (CrossingType a, CrossingType b) => (CrossingState a -> CrossingState b) -> k a -> k b

	crossingOwner     :: c ct -> k ct
	crossingIndex     :: c ct -> Int
	crossingState     :: (CrossingType ct) => c ct -> CrossingState ct
	nthIncidentDart   :: c ct -> Int -> d ct

	nextCW, nextCCW   :: d ct -> d ct
	opposite          :: d ct -> d ct
	incidentCrossing  :: d ct -> c ct
	dartPlace         :: d ct -> Int
	dartOwner         :: d ct -> k ct

	dartArrIndex           :: d ct -> Int
	forMIncidentDarts      :: (Monad m) => c ct -> (d ct -> m ()) -> m ()
	foldMIncidentDarts     :: (Monad m) => c ct -> (d ct -> s -> m s) -> s -> m s
	foldMIncidentDartsFrom :: (Monad m) => d ct -> RotationDirection -> (d ct -> s -> m s) -> s -> m s


	forMIncidentDarts c f = mapM_ f $ incidentDarts c
	foldMIncidentDarts c f s = f (nthIncidentDart c 0) s >>= f (nthIncidentDart c 1) >>= f (nthIncidentDart c 2) >>= f (nthIncidentDart c 3)
	foldMIncidentDartsFrom dart !direction f s =
		let	c = incidentCrossing dart
			i = dartPlace dart
			d = directionSign direction
		in f dart s >>= f (nthIncidentDart c $! (i + d) .&. 3) >>= f (nthIncidentDart c $! (i + 2 * d) .&. 3) >>= f (nthIncidentDart c $! (i + 3 * d) .&. 3)


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
allCrossings t = map (nthCrossing t) [1 .. numberOfCrossings t]


{-# INLINE allDarts #-}
allDarts :: (Knotted k c d) => k ct -> [d ct]
allDarts = concatMap incidentDarts . allCrossings


{-# INLINE forMAdjacentDarts #-}
forMAdjacentDarts :: (Monad m, Knotted k c d) => c ct -> (d ct -> m ()) -> m ()
forMAdjacentDarts c f = forMIncidentDarts c (f . opposite)


{-# INLINE foldMAdjacentDarts #-}
foldMAdjacentDarts :: (Monad m, Knotted k c d) => c ct -> (d ct -> s -> m s) -> s -> m s
foldMAdjacentDarts c f = foldMIncidentDarts c (f . opposite)


{-# INLINE foldMAdjacentDartsFrom #-}
foldMAdjacentDartsFrom :: (Monad m, Knotted k c d) => d ct -> RotationDirection -> (d ct -> s -> m s) -> s -> m s
foldMAdjacentDartsFrom dart direction f = foldMIncidentDartsFrom dart direction (f . opposite)
