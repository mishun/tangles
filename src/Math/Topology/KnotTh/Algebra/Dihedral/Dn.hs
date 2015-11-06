{-# LANGUAGE MultiParamTypeClasses, TypeFamilies #-}
module Math.Topology.KnotTh.Algebra.Dihedral.Dn
    ( module Math.Topology.KnotTh.Algebra
    , module Math.Topology.KnotTh.Algebra.Dihedral
    , Dn
    , fromRotation
    , fromReflectionRotation
    , fromRotationReflection
    , singleElementSubGroup
    , maximumSubGroup
    , fromPeriod
    , fromPeriodAndMirroredZero
    , hasReflectionPart
    , mirroredZero
    , rotationBasis
    , reflectionBasis
    , addSymmetryToSubGroup
    ) where

import Text.Printf
import Math.Topology.KnotTh.Algebra
import Math.Topology.KnotTh.Algebra.Dihedral


-- Dn = (E^mirror) ∘ (C^rotation)
data Dn = D {-# UNPACK #-} !Int {-# UNPACK #-} !Int !Bool
    deriving (Eq, Show)

instance RotationAction Dn where
    rotationOrder (D n _ _) = n

    rotateByUnchecked rot (D n r m) | not m      = D n ((r + rot) `mod` n) False
                                    | otherwise  = D n ((r - rot) `mod` n) True

instance MirrorAction Dn where
    mirrorIt (D n r m) = D n r (not m)

instance Composition Dn where
    {-# INLINE (∘) #-}
    D n1 r1 m1 ∘ D n0 r0 m0 | n0 /= n1   = error $ printf "(∘): Dn order conflict: %i and %i" n1 n0
                            | not m0     = D n0 ((r0 + r1) `mod` n0) m1
                            | otherwise  = D n0 ((r0 - r1) `mod` n0) (not m1)

instance Group Dn where
    data SubGroup Dn = Per {-# UNPACK #-} !Int {-# UNPACK #-} !Int
                     | Mir {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int
        deriving (Eq, Show)

    {-# INLINE inverse #-}
    inverse g@(D _ _ True ) = g
    inverse g@(D _ 0 False) = g
    inverse   (D n r False) = D n (n - r) False

instance RotationGroup Dn where
    {-# INLINE identity #-}
    identity n | n <= 0     = error $ printf "identity: order %i is non-positive" n
               | otherwise  = D n 0 False

    {-# INLINE pointsUnderSub #-}
    pointsUnderSub (Per n _)   = n
    pointsUnderSub (Mir n _ _) = n

    {-# INLINE rotation #-}
    rotation (D _ r _) = r

    {-# INLINE rotationPeriod #-}
    rotationPeriod (Per _ p)   = p
    rotationPeriod (Mir _ p _) = p

    {-# INLINE permutePoint #-}
    permutePoint (D n r True ) i = (-i - r) `mod` n
    permutePoint (D n r False) i = ( i + r) `mod` n

instance DihedralGroup Dn where
    reflection (D _ _ m) = m


{-# INLINE fromRotation #-}
fromRotation :: Int -> Int -> Dn
fromRotation n p | n <= 0     = error $ printf "fromRotation: order %i is non-positive" n
                 | otherwise  = D n (p `mod` n) False


{-# INLINE fromReflectionRotation #-}
fromReflectionRotation :: Int -> (Bool, Int) -> Dn
fromReflectionRotation n (m, r) | n <= 0     = error $ printf "fromReflectionRotation: order %i is non-positive" n
                                | otherwise  = D n (r `mod` n) m


{-# INLINE fromRotationReflection #-}
fromRotationReflection :: Int -> (Int, Bool) -> Dn
fromRotationReflection n (r, m) | n <= 0     = error $ printf "fromRotationReflection: order %i is non-positive" n
                                | not m      = D n (  r  `mod` n) m
                                | otherwise  = D n ((-r) `mod` n) m



singleElementSubGroup :: Int -> SubGroup Dn
singleElementSubGroup n | n <= 0     = error $ printf "singleElementSubGroup: order %i is non-positive" n
                        | otherwise  = Per n n


maximumSubGroup :: Int -> SubGroup Dn
maximumSubGroup n | n <= 0     = error $ printf "maximumSubGroup: order %i is non-positive" n
                  | otherwise  = Mir n 1 0


fromPeriod :: Int -> Int -> SubGroup Dn
fromPeriod !n !p | n <= 0        = error $ printf "fromPeriod: order %i is non-positive" n
                 | p <= 0        = error $ printf "fromPeriod: period %i is non-positive" p
                 | mod n p /= 0  = error $ printf "fromPeriod: period %i does not divide order %i" p n
                 | otherwise     = Per n p


fromPeriodAndMirroredZero :: Int -> Int -> Int -> SubGroup Dn
fromPeriodAndMirroredZero n p mz | n <= 0        = error $ printf "fromPeriodAndMirroredZero: order %i is non-positive" n
                                 | p <= 0        = error $ printf "fromPeriodAndMirroredZero: period %i is non-positive" p
                                 | mod n p /= 0  = error $ printf "fromPeriodAndMirroredZero: period %i does not divide order %i" p n
                                 | otherwise     = Mir n p (mz `mod` p)


hasReflectionPart :: SubGroup Dn -> Bool
hasReflectionPart (Per {})   = False
hasReflectionPart (Mir {}) = True


mirroredZero :: SubGroup Dn -> Maybe Int
mirroredZero (Per _ _)    = Nothing
mirroredZero (Mir _ _ mz) = Just mz


rotationBasis :: SubGroup Dn -> Dn
rotationBasis sg = fromRotation (pointsUnderSub sg) (rotationPeriod sg)


reflectionBasis :: SubGroup Dn -> Dn
reflectionBasis (Mir n _ mz) = fromRotationReflection n (mz, True)
reflectionBasis (Per _ _)    = error "reflectionBasis: no reflection"


addSymmetryToSubGroup :: SubGroup Dn -> Dn -> SubGroup Dn
addSymmetryToSubGroup s (D n _ _) | n' /= n  = error $ printf "addSymmetryToSubGroup: order conflict: %i and %i" n' n
    where n' = pointsUnderSub s
addSymmetryToSubGroup (Per n p   ) (D _ r False) = fromPeriod n (gcd p r)
addSymmetryToSubGroup (Per n p   ) (D _ r True ) = fromPeriodAndMirroredZero n p (-r)
addSymmetryToSubGroup (Mir n p mz) (D _ r False) = fromPeriodAndMirroredZero n (gcd p r) mz
addSymmetryToSubGroup (Mir n p mz) (D _ r True ) = fromPeriodAndMirroredZero n (gcd p (mz + r)) mz


instance GroupAction Dn Dn where
    {-# INLINE transform #-}
    transform (D n1 r1 m1) (D n0 r0 m0) | n0 /= n1   = error $ printf "(∘): Dn order conflict: %i and %i" n1 n0
                                        | not m0     = D n0 ((r0 + r1) `mod` n0) m1
                                        | otherwise  = D n0 ((r0 - r1) `mod` n0) (not m1)

instance (RotationAction a, MirrorAction a) => GroupAction Dn a where
    {-# INLINE transform #-}
    transform g x | l /= l'       = error $ printf "transform: order conflict (%i is order of object, %i order of group)" l l'
                  | reflection g  = mirrorIt $ rotateBy (rotation g) x
                  | otherwise     = rotateBy (rotation g) x
        where l = rotationOrder x
              l' = rotationOrder g
