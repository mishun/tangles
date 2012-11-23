module Math.KnotTh.Crossings.SubTangle
	( DirectSumDecompositionType(..)
	, SubTangleCrossing
	, subTangle
	, SubTangleTangle
	, fromTangle
	, fromTangle'
	, tangleInside
	, numberOfCrossingsInside
	, isLonerInside
	, numberOfCrossingsAfterSubstitution
	, isCrossingOrientationInverted'
	, subTangleLegFromDart
	, directSumDecompositionTypeInside
	, directSumDecompositionType
	, substitute
	) where

import Data.Array.Unboxed (UArray, (!), listArray)
import Math.Algebra.Group.Dn (DnSubGroup, pointsUnderSubGroup)
import Math.Algebra.Group.D4 ((<*>), ec, D4SubGroup, fromDnSubGroup)
import Math.KnotTh.Knotted
import Math.KnotTh.Tangles


data DirectSumDecompositionType = NonDirectSumDecomposable | DirectSum01_23 | DirectSum12_30 deriving (Eq, Show)


changeSumType :: DirectSumDecompositionType -> DirectSumDecompositionType
changeSumType NonDirectSumDecomposable = NonDirectSumDecomposable
changeSumType DirectSum01_23 = DirectSum12_30
changeSumType DirectSum12_30 = DirectSum01_23


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


instance (Show ct, CrossingType ct) => Show (SubTangleCrossing ct) where
	show cr = concat ["(SubTangle ", show $ _code cr, " ", show $ _symmetry cr, " ", show $ _sumType cr, " (", show $ subTangle cr, "))"]


type SubTangleTangle ct = Tangle (SubTangleCrossing ct)


fromTangle :: (CrossingType ct) => Tangle ct -> DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossing ct
fromTangle tangle symmetry sumType code
	| numberOfLegs tangle /= 4           = error "fromTangle: tangle must have 4 legs"
	| pointsUnderSubGroup symmetry /= 4  = error "fromTangle: symmetry group must have 4 points"
	| otherwise                          = SubTangle
		{ _code     = code
		, _symmetry = fromDnSubGroup symmetry
		, _sumType  = sumType
		, subTangle = tangle
		}


fromTangle' :: (CrossingType ct) => Tangle (SubTangleCrossing ct) -> DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossing ct
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
tangleInside :: (CrossingType ct, Knotted k c d) => c (SubTangleCrossing ct) -> Tangle ct
tangleInside = subTangle . crossingType . crossingState


{-# INLINE numberOfCrossingsInside #-}
numberOfCrossingsInside :: (CrossingType ct, Knotted k c d) => c (SubTangleCrossing ct) -> Int
numberOfCrossingsInside = numberOfCrossings . tangleInside

{-
{-# INLINE numberOfCrossingsInside' #-}
numberOfCrossingsInside' :: (CrossingType ct) => Crossing (SubTangleCrossing ct) -> Int
numberOfCrossingsInside' = numberOfCrossingsInside . crossingState
-}

{-# INLINE isLonerInside #-}
isLonerInside :: (CrossingType ct, Knotted k c d) => c (SubTangleCrossing ct) -> Bool
isLonerInside = (== 1) . numberOfCrossingsInside

{-
{-# INLINE isLoner' #-}
isLoner' :: (CrossingType ct) => Crossing (SubTangleCrossing ct) -> Bool
isLoner' = (== 1) . numberOfCrossingsInside'
-}

numberOfCrossingsAfterSubstitution :: (CrossingType ct) => SubTangleTangle ct -> Int
numberOfCrossingsAfterSubstitution = sum . map numberOfCrossingsInside . allCrossings


{-# INLINE subTangleLegFromDart #-}
subTangleLegFromDart :: (CrossingType ct, Knotted k c d) => d (SubTangleCrossing ct) -> Dart ct
subTangleLegFromDart d = nthLeg (tangleInside $ incidentCrossing d) $! crossingLegIdByDart d


directSumDecompositionTypeInside :: (CrossingType ct, Knotted k c d) => d (SubTangleCrossing ct) -> DirectSumDecompositionType
directSumDecompositionTypeInside d
	| f          = changeSumType st
	| otherwise  = st
	where
		st = _sumType $ crossingType $ crossingState $ incidentCrossing d
		f = isCrossingOrientationInverted (incidentCrossing d) /= odd (crossingLegIdByDart d)


directSumDecompositionType :: (CrossingType ct) => CrossingState (SubTangleCrossing ct) -> DirectSumDecompositionType
directSumDecompositionType c
	| f          = changeSumType st
	| otherwise  = st
	where
		st = _sumType $ crossingType c
		f = isCrossingOrientationInverted' c /= odd (crossingLegIdByDartId c 0)


substitute :: (CrossingType ct) => Tangle (SubTangleCrossing ct) -> Tangle ct
substitute tangle =
	fromLists (map oppositeExt $! allLegs tangle) $!
		let connections b = do
			let rev = isCrossingOrientationInverted b
			!c <- allCrossings $! tangleInside b
			let nb = map (oppositeInt b) $! incidentDarts c
			let st
				| rev        = alterCrossingOrientation (ec <*>) $! crossingState c
				| otherwise  = crossingState c
			return $! (if rev then reverse nb else nb, st)
		in concatMap connections $! allCrossings tangle
	where
		offset :: UArray Int Int
		offset = listArray (1, numberOfCrossings tangle) $! scanl (\ !i !c -> i + numberOfCrossingsInside c) 0 $! allCrossings tangle

		oppositeInt b u
			| isLeg v                          = oppositeExt $! dartByCrossingLegId b (legPlace v)
			| isCrossingOrientationInverted b  = (w, 3 - dartPlace v)
			| otherwise                        = (w, dartPlace v)
			where
				v = opposite u
				c = incidentCrossing v
				w = (offset ! crossingIndex b) + crossingIndex c

		oppositeExt u
			| isLeg v    = (0, legPlace v)
			| otherwise  = oppositeInt c $! subTangleLegFromDart v
			where
				v = opposite u
				c = incidentCrossing v
