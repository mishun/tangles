module Math.Algebra.Group.Dn
    ( Dn
    , pointsUnderGroup
    , rotation
    , reflection
    , identity
    , inverse
    , (∘)
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

import Text.Printf


-- Dn = (E^mirror) * (C^rotation)
data Dn = Dn
    { pointsUnderGroup :: {-# UNPACK #-} !Int
    , rotation         :: {-# UNPACK #-} !Int
    , reflection       :: !Bool
    } deriving (Eq, Show)


{-# INLINE identity #-}
identity :: Int -> Dn
identity n
    | n <= 0     = error $ printf "identity: order %i is non-positive" n
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


{-# INLINE (∘) #-}
(∘) :: Dn -> Dn -> Dn
(∘) g h
    | pointsUnderGroup g /= pointsUnderGroup h  = error $ printf "(*): Dn order conflict: %i and %i" (pointsUnderGroup g) (pointsUnderGroup h)
    | otherwise                                 = Dn
        { pointsUnderGroup = pointsUnderGroup g
        , rotation         = mod (if reflection h then pointsUnderGroup g + rotation h - rotation g else rotation g + rotation h) (pointsUnderGroup g)
        , reflection       = reflection g /= reflection h
        }


{-# INLINE fromRotation #-}
fromRotation :: Int -> Int -> Dn
fromRotation n p
    | n <= 0    = error $ printf "fromRotation: order %i is non-positive" n
    | otherwise = Dn { pointsUnderGroup = n, rotation = mod p n, reflection = False }


{-# INLINE fromReflectionRotation #-}
fromReflectionRotation :: Int -> (Bool, Int) -> Dn
fromReflectionRotation n (m, r)
    | n <= 0     = error $ printf "fromReflectionRotation: order %i is non-positive" n
    | otherwise  = Dn { pointsUnderGroup = n, rotation = mod r n , reflection = m }


{-# INLINE fromRotationReflection #-}
fromRotationReflection :: Int -> (Int, Bool) -> Dn
fromRotationReflection n (r, m)
    | n <= 0     = error $ printf "fromRotationReflection: order %i is non-positive" n
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
    | n <= 0     = error $ printf "singleElementSubGroup: order %i is non-positive" n
    | otherwise  = Rot { pointsUnderSubGroup = n, rotationPeriod = n }


maximumSubGroup :: Int -> DnSubGroup
maximumSubGroup n
    | n <= 0     = error $ printf "maximumSubGroup: order %i is non-positive" n
    | otherwise  = Mirr { pointsUnderSubGroup = n, rotationPeriod = 1, mirroredZero = 0 }


fromPeriod :: Int -> Int -> DnSubGroup
fromPeriod n p
    | n <= 0        = error $ printf "fromPeriod: order %i is non-positive" n
    | p <= 0        = error $ printf "fromPeriod: period %i is non-positive" p
    | mod n p /= 0  = error $ printf "fromPeriod: period %i does not divide order %i" p n
    | otherwise     = Rot { pointsUnderSubGroup = n, rotationPeriod = p }


fromPeriodAndMirroredZero :: Int -> Int -> Int -> DnSubGroup
fromPeriodAndMirroredZero n p mz
    | n <= 0        = error $ printf "fromPeriodAndMirroredZero: order %i is non-positive" n
    | p <= 0        = error $ printf "fromPeriodAndMirroredZero: period %i is non-positive" p
    | mod n p /= 0  = error $ printf "fromPeriodAndMirroredZero: period %i does not divide order %i" p n
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
    | n /= pointsUnderGroup g  = error $ printf "addSymmetryToSubGroup: order conflict: %i and %i" n (pointsUnderGroup g)
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
