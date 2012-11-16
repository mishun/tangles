{-# LANGUAGE UnboxedTuples #-}
module Math.KnotTh.Crossings
	( CrossingType(..)
	, CrossingState
	, crossingOrientation
	, crossingType
	, crossing
	, crossing'
	, modifyCrossingOrientation
	, isCrossingOrientationInverted
	, crossingLegByDart
	, crossingDartByLeg
	, crossingCode
	) where

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
		in map (crossing ct) $!
			case extra of
				Nothing -> orient
				Just h  -> filter (\ !g -> equivalenceClassId s g <= equivalenceClassId s (h <*> g)) orient


data CrossingState ct = Crossing
	{ code                :: {-# UNPACK #-} !Int
	, crossingOrientation :: {-# UNPACK #-} !D4
	, symmetry            :: !D4SubGroup
	, crossingType        :: !ct
	}


instance (Eq ct) => Eq (CrossingState ct) where
	(==) a b = symmetry a == symmetry b
		&& equivalenceClassId (symmetry a) (crossingOrientation a) == equivalenceClassId (symmetry b) (crossingOrientation b)
		&& crossingType a == crossingType b


instance (NFData ct) => NFData (CrossingState ct) where
	rnf cr = rnf (crossingType cr) `seq` cr `seq` ()


instance (Show ct) => Show (CrossingState ct) where
	show c = concat [ "(" , show $ crossingOrientation c, " ", show $ crossingType c, ")"]


{-# INLINE crossing #-}
crossing :: (CrossingType ct) => ct -> D4 -> CrossingState ct
crossing !ct !g = Crossing
	{ code                = crossingTypeCode ct
	, crossingOrientation = g
	, symmetry            = localCrossingSymmetry ct
	, crossingType        = ct
	}


{-# INLINE crossing' #-}
crossing' :: (CrossingType ct) => ct -> CrossingState ct
crossing' !ct = Crossing
	{ code                = crossingTypeCode ct
	, crossingOrientation = i
	, symmetry            = localCrossingSymmetry ct
	, crossingType        = ct
	}


modifyCrossingOrientation :: D4 -> CrossingState ct -> CrossingState ct
modifyCrossingOrientation g st = Crossing
	{ code                = code st
	, crossingOrientation = g <*> crossingOrientation st
	, symmetry            = symmetry st
	, crossingType        = crossingType st
	}


{-# INLINE isCrossingOrientationInverted #-}
isCrossingOrientationInverted :: CrossingState ct -> Bool
isCrossingOrientationInverted = hasReflection . crossingOrientation


{-# INLINE crossingLegByDart #-}
crossingLegByDart :: CrossingState ct -> Int -> Int
crossingLegByDart cr = permute (inverse $! crossingOrientation cr)


{-# INLINE crossingDartByLeg #-}
crossingDartByLeg :: CrossingState ct -> Int -> Int
crossingDartByLeg cr = permute (crossingOrientation cr)


{-# INLINE crossingCode #-}
crossingCode :: CrossingState ct -> RotationDirection -> Int -> (# Int, Int #)
crossingCode cr dir p =
	let t = fromReflectionRotation (isClockwise dir) (-p) <*> crossingOrientation cr
	in (# code cr, equivalenceClassId (symmetry cr) t #)
