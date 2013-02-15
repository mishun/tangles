module Math.KnotTh.Tangle.TangleDefinition.Class
    ( TangleLike(..)
    , firstLeg
    , lastLeg
    , nextLeg
    , allLegOpposites
    ) where

import Math.KnotTh.Knotted


class (Knotted tangle cross dart) => TangleLike tangle cross dart | tangle -> cross, cross -> dart, dart -> tangle where
    numberOfLegs :: tangle ct -> Int
    allLegs      :: tangle ct -> [dart ct]
    nthLeg       :: tangle ct -> Int -> dart ct
    isLeg        :: dart ct -> Bool
    legPlace     :: dart ct -> Int


{-# INLINE firstLeg #-}
firstLeg :: (TangleLike t c d) => t ct -> d ct
firstLeg t = nthLeg t 0


{-# INLINE lastLeg #-}
lastLeg :: (TangleLike t c d) => t ct -> d ct
lastLeg t = nthLeg t (-1)


{-# INLINE nextLeg #-}
nextLeg :: (TangleLike t c d) => Int -> d ct -> d ct
nextLeg n d = nthLeg (dartOwner d) (legPlace d + n)


{-# INLINE allLegOpposites #-}
allLegOpposites :: (TangleLike t c d) => t ct -> [d ct]
allLegOpposites = map opposite . allLegs
