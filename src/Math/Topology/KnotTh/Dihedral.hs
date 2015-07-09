{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Dihedral
    ( RotationAction(..)
    , allRotationsOf
    , naivePeriodOf
    , MirrorAction(..)
    , allOrientationsOf
    , RotationDirection
    , cw
    , ccw
    , bothDirections
    , isCounterClockwise
    , isClockwise
    , directionSign
    , Composition(..)
    , TensorProduct(..)
    , Group(..)
    , power
    , RotationGroup(..)
    , DihedralGroup(..)
    , GroupAction(..)
    ) where

import Control.Monad ((>=>))


class RotationAction a where
    rotationOrder     :: a -> Int
    rotateBy          :: Int -> a -> a

    -- | Never call it
    rotateByUnchecked :: Int -> a -> a

    {-# INLINE rotateBy #-}
    rotateBy !rot x | l == 0            = x
                    | rot `mod` l == 0  = x
                    | otherwise         = rotateByUnchecked rot x
        where l = rotationOrder x

    rotateByUnchecked = rotateBy

allRotationsOf :: (RotationAction a) => a -> [a]
allRotationsOf a =
    case rotationOrder a of
        0 -> [a]
        n -> map (flip rotateBy a) [0 .. n - 1]

naivePeriodOf :: (Eq a, RotationAction a) => a -> Int
naivePeriodOf x | rotationOrder x == 0  = 0
                | otherwise             = head $ filter ((== x) . flip rotateBy x) [1 ..]


class MirrorAction a where
    mirrorIt :: a -> a

allOrientationsOf :: (RotationAction a, MirrorAction a) => a -> [a]
allOrientationsOf = allRotationsOf >=> (\ t -> [t, mirrorIt t])


newtype RotationDirection = RD Int deriving (Eq, Ord)

instance Show RotationDirection where
    show d | isCounterClockwise d  = "CCW"
           | otherwise             = "CW"

instance MirrorAction RotationDirection where
    mirrorIt (RD d) = RD (-d)

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


class Composition a where
    (∘) :: a -> a -> a

instance Composition (a -> a) where
    (∘) = (.)


class TensorProduct a where
    (⊗) :: a -> a -> a


class (Composition g) => Group g where
    data SubGroup g :: *

    inverse :: g -> g

power :: (RotationGroup g) => Int -> g -> g
power =
    let go 0 _ !r              = r
        go n x !r | even n     = go (n `div` 2) (x ∘ x) r
                  | otherwise  = go (n `div` 2) (x ∘ x) (x ∘ r)
    in \ n x ->
        if n >= 0
            then go n x (identity $ rotationOrder x)
            else go (-n) (inverse x) (identity $ rotationOrder x)


class (Group g, RotationAction g) => RotationGroup g where
    identity         :: Int -> g
    pointsUnderSub   :: SubGroup g -> Int
    rotation         :: g -> Int
    rotationPeriod   :: SubGroup g -> Int
    permutePoint     :: g -> Int -> Int

class (RotationGroup g, MirrorAction g) => DihedralGroup g where
    reflection :: g -> Bool


class (Group g) => GroupAction g a where
    transform :: g -> a -> a
