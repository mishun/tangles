{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Dihedral
    ( Group(..)
    , power
    , RotationGroup(..)
    , DihedralGroup(..)
    , RotationAction(..)
    , rotateBy
    , allRotationsOf
    , DihedralAction(..)
    , allOrientationsOf
    , GroupAction(..)
    , RotationDirection
    , cw
    , ccw
    , bothDirections
    , isCounterClockwise
    , isClockwise
    , directionSign
    , oppositeDirection
    ) where

import Control.Monad ((>=>))

class Group g where
    data SubGroup g :: *

    inverse :: g -> g
    (∘)     :: g -> g -> g

power :: (RotationGroup g) => Int -> g -> g
power =
    let go 0 _ !r              = r
        go n x !r | even n     = go (n `div` 2) (x ∘ x) r
                  | otherwise  = go (n `div` 2) (x ∘ x) (x ∘ r)
    in \ n x ->
        if n >= 0
            then go n x (identity $ pointsUnderGroup x)
            else go (-n) (inverse x) (identity $ pointsUnderGroup x)


class (Group g) => RotationGroup g where
    identity         :: Int -> g
    pointsUnderGroup :: g -> Int
    pointsUnderSub   :: SubGroup g -> Int
    rotation         :: g -> Int
    rotationPeriod   :: SubGroup g -> Int
    permutePoint     :: g -> Int -> Int

class (RotationGroup g) => DihedralGroup g where
    reflection :: g -> Bool


class RotationAction a where
    rotationOrder     :: a -> Int
    rotateByUnchecked :: Int -> a -> a


{-# INLINE rotateBy #-}
rotateBy :: (RotationAction a) => Int -> a -> a
rotateBy !rot x | l == 0            = x
                | rot `mod` l == 0  = x
                | otherwise         = rotateByUnchecked rot x
    where l = rotationOrder x

allRotationsOf :: (RotationAction a) => a -> [a]
allRotationsOf a =
    case rotationOrder a of
        0 -> [a]
        n -> map (flip rotateBy a) [0 .. n - 1]


class (RotationAction a) => DihedralAction a where
    mirrorIt :: a -> a

allOrientationsOf :: (DihedralAction a) => a -> [a]
allOrientationsOf = allRotationsOf >=> (\ t -> [t, mirrorIt t])


class GroupAction a where
    type TransformGroup a :: *

    transform :: TransformGroup a -> a -> a


newtype RotationDirection = RD Int deriving (Eq, Ord)


instance Show RotationDirection where
    show d | isCounterClockwise d  = "CCW"
           | otherwise             = "CW"


cw :: RotationDirection
cw = RD (-1)


ccw :: RotationDirection
ccw = RD 1


bothDirections :: [RotationDirection]
bothDirections = [cw, ccw]


{-# INLINE isCounterClockwise #-}
isCounterClockwise :: RotationDirection -> Bool
isCounterClockwise (RD d) = d > 0


{-# INLINE isClockwise #-}
isClockwise :: RotationDirection -> Bool
isClockwise (RD d) = d < 0


{-# INLINE directionSign #-}
directionSign :: RotationDirection -> Int
directionSign (RD d) = d


{-# INLINE oppositeDirection #-}
oppositeDirection :: RotationDirection -> RotationDirection
oppositeDirection (RD d) = RD (-d)
