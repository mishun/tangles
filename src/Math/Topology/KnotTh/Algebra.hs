{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Algebra
    ( Composition(..)
    , Group(..)
    , GroupAction(..)
    , TensorProduct(..)
    , extendedGCD
    ) where


class Composition a where
    (∘) :: a -> a -> a

instance Composition (a -> a) where
    (∘) = (.)


class (Composition g) => Group g where
    data SubGroup g :: *

    inverse :: g -> g


class (Group g) => GroupAction g a where
    transform :: g -> a -> a


class TensorProduct a where
    (⊗) :: a -> a -> a


extendedGCD :: (Integral a) => a -> a -> (a, a, a)
extendedGCD !a 0 | a >= 0     = (a, 1, 0)
                 | otherwise  = (-a, -1, 0)
extendedGCD !a !b =
    let (quotient, remainder) = divMod a b
        (g, x, y) = extendedGCD b remainder
    in (g, y, x - quotient * y)
