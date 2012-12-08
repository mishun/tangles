module Math.Algebra.Group.Dn
	( Dn
	, pointsUnderGroup
	, rotation
	, reflection
	, identity
	, inverse
	, (<*>)
	, fromRotation
	, fromReflectionRotation
	, fromRotationReflection
	, permute

	, DnSubGroup
	, pointsUnderSubGroup
	, rotationPeriod
	, mirroredZero
	, singleElementSubGroup
	, maximumSubGroup
	, fromPeriod
	, fromPeriodAndMirroredZero
	, hasReflectionPart
	, rotationBasis
	, reflectionBasis
	, addSymmetryToSubGroup
	) where


-- Dn = (E^mirror) * (C^rotation)
data Dn = Dn
	{ pointsUnderGroup :: {-# UNPACK #-} !Int
	, rotation         :: {-# UNPACK #-} !Int
	, reflection       :: !Bool
	} deriving (Eq, Show)


{-# INLINE identity #-}
identity :: Int -> Dn
identity n
	| n <= 0     = error "identity: order is non-positive"
	| otherwise  = Dn { pointsUnderGroup = n, rotation = 0, reflection = False }


{-# INLINE inverse #-}
inverse :: Dn -> Dn
inverse g
	| reflection g || rotation g == 0  = g
	| otherwise                        =
		Dn
		{ pointsUnderGroup = pointsUnderGroup g
		, rotation         = pointsUnderGroup g - rotation g
		, reflection       = False
		}


{-# INLINE (<*>) #-}
(<*>) :: Dn -> Dn -> Dn
(<*>) g h
	| pointsUnderGroup g /= pointsUnderGroup h  = error "(*): Dn order conflict"
	| otherwise                                 = Dn
		{ pointsUnderGroup = pointsUnderGroup g
		, rotation         = mod (if reflection h then pointsUnderGroup g + rotation h - rotation g else rotation g + rotation h) (pointsUnderGroup g)
		, reflection       = reflection g /= reflection h
		}


{-# INLINE fromRotation #-}
fromRotation :: Int -> Int -> Dn
fromRotation n p
	| n <= 0    = error "fromRotation: order is non-positive"
	| otherwise = Dn { pointsUnderGroup = n, rotation = mod p n, reflection = False }


{-# INLINE fromReflectionRotation #-}
fromReflectionRotation :: Int -> (Bool, Int) -> Dn
fromReflectionRotation n (m, r)
	| n <= 0     = error "fromReflectionRotation: order is non-positive"
	| otherwise  = Dn { pointsUnderGroup = n, rotation = mod r n , reflection = m }


{-# INLINE fromRotationReflection #-}
fromRotationReflection :: Int -> (Int, Bool) -> Dn
fromRotationReflection n (r, m)
	| n <= 0     = error "fromRotationReflection: order is non-positive"
	| otherwise  = Dn { pointsUnderGroup = n, rotation = mod (if m then -r else r) n, reflection = m }


{-# INLINE permute #-}
permute :: Dn -> Int -> Int
permute g i
	| reflection g  = (p - (i + rotation g)) `mod` p
	| otherwise     = (p + (i + rotation g)) `mod` p
	where
		p = pointsUnderGroup g


data DnSubGroup =
	Rot
	{ pointsUnderSubGroup :: {-# UNPACK #-} !Int
	, rotationPeriod      :: {-# UNPACK #-} !Int
	}
	| Mirr
	{ pointsUnderSubGroup :: {-# UNPACK #-} !Int
	, rotationPeriod      :: {-# UNPACK #-} !Int
	, mirroredZero        :: {-# UNPACK #-} !Int
	}
	deriving (Eq, Show)


singleElementSubGroup :: Int -> DnSubGroup
singleElementSubGroup n
	| n <= 0     = error "singleElementSubGroup: order is non-positive"
	| otherwise  = Rot { pointsUnderSubGroup = n, rotationPeriod = n }


maximumSubGroup :: Int -> DnSubGroup
maximumSubGroup n
	| n <= 0     = error "maximumSubGroup: order is non-positive"
	| otherwise  = Mirr { pointsUnderSubGroup = n, rotationPeriod = 1, mirroredZero = 0 }


fromPeriod :: Int -> Int -> DnSubGroup
fromPeriod n p
	| n <= 0        = error "fromPeriod: order is non-positive"
	| p <= 0        = error "fromPeriod: period is non-positive"
	| mod n p /= 0  = error "fromPeriod: period does not divide order"
	| otherwise     = Rot { pointsUnderSubGroup = n, rotationPeriod = p }


fromPeriodAndMirroredZero :: Int -> Int -> Int -> DnSubGroup
fromPeriodAndMirroredZero n p mz
	| n <= 0        = error "fromPeriodAndMirroredZero: order is non-positive"
	| p <= 0        = error "fromPeriodAndMirroredZero: period is non-positive"
	| mod n p /= 0  = error "fromPeriodAndMirroredZero: period does not divide order"
	| otherwise     = Mirr { pointsUnderSubGroup = n, rotationPeriod = p, mirroredZero = mod mz p }


hasReflectionPart :: DnSubGroup -> Bool
hasReflectionPart (Rot _ _) = False
hasReflectionPart _ = True


rotationBasis :: DnSubGroup -> Dn
rotationBasis sg = fromRotation (pointsUnderSubGroup sg) (rotationPeriod sg)


reflectionBasis :: DnSubGroup -> Dn
reflectionBasis sg = fromRotationReflection (pointsUnderSubGroup sg) (mirroredZero sg, True)


addSymmetryToSubGroup :: DnSubGroup -> Dn -> DnSubGroup
addSymmetryToSubGroup s g
	| n /= pointsUnderGroup g  = error "addSymmetryToSubGroup: order conflict"
	| g == identity n          = s
	| not ms && not mg         = fromPeriod n p
	| not ms && mg             = fromPeriodAndMirroredZero n (rotationPeriod s) (-rotation g)
	| ms && not mg             = fromPeriodAndMirroredZero n p (mirroredZero s)
	| otherwise                = fromPeriodAndMirroredZero n (gcd (rotationPeriod s) (mirroredZero s + rotation g)) (mirroredZero s)
	where
		n = pointsUnderSubGroup s
		ms = hasReflectionPart s
		mg = reflection g
		p = gcd (rotationPeriod s) (rotation g)

{-
adjointDifferenceForBasis :: (Group f e) => (e, e) -> SubGroup -> Element -> Element -> e
adjointDifferenceForBasis (adjRot, adjMir) sg a b
	| (group a /= D n) || (group b /= D n)  = error "adjointDifferenceForBasis: order conflict"
	| mod rotationDiff period /= 0          = error "adjointDifferenceForBasis: elements are not equivalent"
	| otherwise  =
		if needMirror
			then adjRotation <*> adjMir
			else adjRotation

	where
		n = groupSetOrder' sg

		period = rotationPeriod sg

		needMirror = (hasReflection a) /= (hasReflection b)

		toRotate =
			if needMirror
				then (reflectionBasis sg) <*> b
				else b

		rotationDiff = (rotation a) - (rotation toRotate)

		rotationNum =
			let d = div rotationDiff period
			in if hasReflection a then -d else d

		adjRotation = power rotationNum adjRot
-}
