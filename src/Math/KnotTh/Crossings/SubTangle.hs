module Math.KnotTh.Crossings.SubTangle
	( module Math.KnotTh.Crossings
	, DirectSumDecompositionType(..)
	, SubTangleCrossing
	, subTangle
	, SubTangleTangle
	, fromTangle
	, fromTangle'
	, tangleInside
	, tangleInside'
	, numberOfCrossingsInside
	, numberOfCrossingsInside'
	, isLoner
	, isLoner'
	, numberOfCrossingsAfterSubstitution
	, isCrossingOrientationInverted'
	, subTangleLegFromDart
	, directSumDecompositionType'
	, directSumDecompositionTypeById
	, directSumDecompositionType
	, substitute
	) where

import Data.Array.Unboxed (UArray, (!), listArray)
import Math.Algebra.Group.Dn (DnSubGroup, pointsUnderSubGroup)
import Math.Algebra.Group.D4 (ec, D4SubGroup, fromDnSubGroup)
import Math.KnotTh.Crossings
import Math.KnotTh.Tangles


data DirectSumDecompositionType = NonDirectSumDecomposable | DirectSum01_23 | DirectSum12_30 deriving (Eq, Show)


data SubTangleCrossing ct = SubTangle
	{ _code     :: {-# UNPACK #-} !Int
	, _symmetry :: !D4SubGroup
	, _sumType  :: !DirectSumDecompositionType
	, subTangle :: Tangle ct
	}


instance Eq (SubTangleCrossing ct) where
	(==) a b = (_code a == _code b)


instance (CrossingType ct) => CrossingType (SubTangleCrossing ct) where
	crossingTypeCode c = _code c

	localCrossingSymmetry c = _symmetry c


instance (Show ct) => Show (SubTangleCrossing ct) where
	show cr = concat ["(SubTangle ", show $ _code cr, " ", show $ _symmetry cr, " ", show $ _sumType cr, " (", show $ subTangle cr, "))"]


type SubTangleTangle ct = Tangle (SubTangleCrossing ct)


fromTangle :: Tangle ct -> DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossing ct
fromTangle tangle symmetry sumType code
	| numberOfLegs tangle /= 4           = error "fromTangle: tangle must have 4 legs"
	| pointsUnderSubGroup symmetry /= 4  = error "fromTangle: symmetry group must have 4 points"
	| otherwise                          = SubTangle
		{ _code     = code
		, _symmetry = fromDnSubGroup symmetry
		, _sumType  = sumType
		, subTangle = tangle
		}


fromTangle' :: Tangle (SubTangleCrossing ct) -> DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossing ct
fromTangle' tangle symmetry sumType code
	| numberOfLegs tangle /= 4           = error "fromTangle': tangle must have 4 legs"
	| pointsUnderSubGroup symmetry /= 4  = error "fromTangle': symmetry group must have 4 points"
	| otherwise                          = SubTangle
		{ _code     = code
		, _symmetry = fromDnSubGroup symmetry
		, _sumType  = sumType
		, subTangle = substitute tangle
		}


{-# INLINE tangleInside #-}
tangleInside :: CrossingState (SubTangleCrossing ct) -> Tangle ct
tangleInside = subTangle . crossingType


{-# INLINE tangleInside' #-}
tangleInside' :: Crossing (SubTangleCrossing ct) -> Tangle ct
tangleInside' = tangleInside . crossingState


{-# INLINE numberOfCrossingsInside #-}
numberOfCrossingsInside :: CrossingState (SubTangleCrossing ct) -> Int
numberOfCrossingsInside = numberOfCrossings . tangleInside


{-# INLINE numberOfCrossingsInside' #-}
numberOfCrossingsInside' :: Crossing (SubTangleCrossing ct) -> Int
numberOfCrossingsInside' = numberOfCrossingsInside . crossingState


{-# INLINE isLoner #-}
isLoner :: CrossingState (SubTangleCrossing ct) -> Bool
isLoner = (== 1) . numberOfCrossingsInside


{-# INLINE isLoner' #-}
isLoner' :: Crossing (SubTangleCrossing ct) -> Bool
isLoner' = (== 1) . numberOfCrossingsInside'


numberOfCrossingsAfterSubstitution :: SubTangleTangle ct -> Int
numberOfCrossingsAfterSubstitution tangle = sum $! map numberOfCrossingsInside' $! allCrossings tangle


{-# INLINE isCrossingOrientationInverted' #-}
isCrossingOrientationInverted' :: Crossing (SubTangleCrossing ct) -> Bool
isCrossingOrientationInverted' = isCrossingOrientationInverted . crossingState


{-# INLINE subTangleLegFromDart #-}
subTangleLegFromDart :: Dart (SubTangleCrossing ct) -> Dart ct
subTangleLegFromDart d =
	let c = crossingState $! incidentCrossing d
	in nthLeg (tangleInside c) $! crossingLegByDart c $! dartPlace d


{-# INLINE directSumDecompositionType' #-}
directSumDecompositionType' :: CrossingState (SubTangleCrossing ct) -> DirectSumDecompositionType
directSumDecompositionType' = _sumType . crossingType


directSumDecompositionTypeById :: CrossingState (SubTangleCrossing ct) -> Int -> DirectSumDecompositionType
directSumDecompositionTypeById cr p =
	case directSumDecompositionType' cr of
		NonDirectSumDecomposable   -> NonDirectSumDecomposable
		DirectSum01_23 | f         -> DirectSum01_23
		               | otherwise -> DirectSum12_30
		DirectSum12_30 | f         -> DirectSum12_30
		               | otherwise -> DirectSum01_23
	where
		f = isCrossingOrientationInverted cr == odd (crossingLegByDart cr p)


directSumDecompositionType :: Dart (SubTangleCrossing ct) -> DirectSumDecompositionType
directSumDecompositionType d = directSumDecompositionTypeById (crossingState $! incidentCrossing d) $! dartPlace d


substitute :: Tangle (SubTangleCrossing ct) -> Tangle ct
substitute tangle =
	fromLists (map oppositeExt $! allLegs tangle) $!
		let connections b = do
			let rev = isCrossingOrientationInverted' b
			!c <- allCrossings $! tangleInside' b
			let nb = map (oppositeInt b) $! incidentDarts c
			let st
				| rev        = modifyCrossingOrientation ec $! crossingState c
				| otherwise  = crossingState c
			return $! (if rev then reverse nb else nb, st)
		in concatMap connections $! allCrossings tangle
	where
		offset :: UArray Int Int
		offset = listArray (1, numberOfCrossings tangle) $! scanl (\ !i !c -> i + numberOfCrossingsInside' c) 0 $! allCrossings tangle

		oppositeInt b u
			| isLeg v    = oppositeExt $! nthIncidentDart b $! crossingDartByLeg (crossingState b) $! legPlace v
			| otherwise  = ((offset ! crossingIndex b) + crossingIndex c, if isCrossingOrientationInverted' b then 3 - dartPlace v else dartPlace v)
			where
				v = opposite u
				c = incidentCrossing v

		oppositeExt u
			| isLeg v    = (0, legPlace v)
			| otherwise  = oppositeInt c $! subTangleLegFromDart v
			where
				v = opposite u
				c = incidentCrossing v
