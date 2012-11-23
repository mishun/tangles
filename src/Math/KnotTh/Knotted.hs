{-# LANGUAGE UnboxedTuples, KindSignatures, FunctionalDependencies #-}
module Math.KnotTh.Knotted
	( CrossingType(..)
	, CrossingState
	, crossingType
	, crossingType'
	, isCrossingOrientationInverted
	, isCrossingOrientationInverted'
	, crossingLegIdByDartId
	, crossingLegIdByDart
	, dartIdByCrossingLegId
	, dartByCrossingLegId
	, alterCrossingOrientation
	, makeCrossing
	, makeCrossing'
	, crossingCode

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
import Control.DeepSeq
import Math.Algebra.RotationDirection
import Math.Algebra.Group.D4 (D4, D4SubGroup, i, inverse, (<*>), hasReflection, permute, fromReflectionRotation, equivalenceClassId, equvalenceClassRepresentatives)


class (Eq ct) => CrossingType ct where
	crossingTypeCode      :: ct -> Int
	localCrossingSymmetry :: ct -> D4SubGroup
	possibleOrientations  :: ct -> Maybe D4 -> [CrossingState ct]

	crossingTypeCode _ = 1

	possibleOrientations ct extra =
		let	s = localCrossingSymmetry ct
			orient = equvalenceClassRepresentatives s
		in map (makeCrossing ct) $!
			case extra of
				Nothing -> orient
				Just h  -> filter (\ !g -> equivalenceClassId s g <= equivalenceClassId s (h <*> g)) orient


data CrossingState ct = Crossing
	{ code        :: {-# UNPACK #-} !Int
	, orientation :: {-# UNPACK #-} !D4
	, symmetry    :: !D4SubGroup
	, stateType   :: !ct
	}


instance (Eq ct) => Eq (CrossingState ct) where
	(==) a b = symmetry a == symmetry b
		&& equivalenceClassId (symmetry a) (orientation a) == equivalenceClassId (symmetry b) (orientation b)
		&& stateType a == stateType b


instance (NFData ct) => NFData (CrossingState ct) where
	rnf cr = rnf (stateType cr) `seq` cr `seq` ()


instance (Show ct) => Show (CrossingState ct) where
	show c = concat [ "(" , show (orientation c), " ", show (stateType c), ")"]


{-# INLINE crossingType #-}
crossingType :: (CrossingType ct, Knotted k c d) => c ct -> ct
crossingType = stateType . crossingState


{-# INLINE crossingType' #-}
crossingType' :: (CrossingType ct) => CrossingState ct -> ct
crossingType' = stateType


{-# INLINE isCrossingOrientationInverted #-}
isCrossingOrientationInverted :: (CrossingType ct, Knotted k c d) => c ct -> Bool
isCrossingOrientationInverted = hasReflection . orientation . crossingState


{-# INLINE isCrossingOrientationInverted' #-}
isCrossingOrientationInverted' :: CrossingState ct -> Bool
isCrossingOrientationInverted' = hasReflection . orientation


{-# INLINE crossingLegIdByDartId #-}
crossingLegIdByDartId :: CrossingState ct -> Int -> Int
crossingLegIdByDartId cr = permute (inverse $! orientation cr)


{-# INLINE crossingLegIdByDart #-}
crossingLegIdByDart :: (CrossingType ct, Knotted t c d) => d ct -> Int
crossingLegIdByDart d = crossingLegIdByDartId (crossingState $ incidentCrossing d) (dartPlace d)


{-# INLINE dartIdByCrossingLegId #-}
dartIdByCrossingLegId :: CrossingState ct -> Int -> Int
dartIdByCrossingLegId cr = permute (orientation cr)


{-# INLINE dartByCrossingLegId #-}
dartByCrossingLegId :: (CrossingType ct, Knotted k c d) => c ct -> Int -> d ct
dartByCrossingLegId c = nthIncidentDart c . dartIdByCrossingLegId (crossingState c)


alterCrossingOrientation :: (D4 -> D4) -> CrossingState ct -> CrossingState ct
alterCrossingOrientation f st = Crossing
	{ code        = code st
	, orientation = f $! orientation st
	, symmetry    = symmetry st
	, stateType   = stateType st
	}


makeCrossing :: (CrossingType ct) => ct -> D4 -> CrossingState ct
makeCrossing !ct !g = Crossing
	{ code        = crossingTypeCode ct
	, orientation = g
	, symmetry    = localCrossingSymmetry ct
	, stateType   = ct
	}


makeCrossing' :: (CrossingType ct) => ct -> CrossingState ct
makeCrossing' ct = makeCrossing ct i


{-# INLINE crossingCode #-}
crossingCode :: (CrossingType ct, Knotted k c d) => RotationDirection -> d ct -> (# Int, Int #)
crossingCode dir d =
	let	p = dartPlace d
		cr = crossingState $! incidentCrossing d
		t = fromReflectionRotation (isClockwise dir) (-p) <*> orientation cr
	in (# code cr, equivalenceClassId (symmetry cr) t #)



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
			p = dartPlace dart
			d = directionSign direction
		in f dart s >>= f (nthIncidentDart c $! (p + d) .&. 3) >>= f (nthIncidentDart c $! (p + 2 * d) .&. 3) >>= f (nthIncidentDart c $! (p + 3 * d) .&. 3)


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
