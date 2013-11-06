module Math.KnotTh.Tangle.Definition.TangleLike
    ( TangleLike(..)
    , firstLeg
    , lastLeg
    , nextLeg
    , allLegOpposites
    ) where

import Math.KnotTh.Knotted


class (Knotted tangle) => TangleLike tangle where
    numberOfLegs   :: tangle ct -> Int
    allLegs        :: tangle ct -> [Dart tangle ct]
    nthLeg         :: tangle ct -> Int -> Dart tangle ct

    isLeg          :: Dart tangle ct -> Bool
    legPlace       :: Dart tangle ct -> Int

    -- | +-------+
    --   |   ^   |
    --   |   |   |
    --   |   |   |
    --   +-------+
    identityTangle :: (CrossingType ct) => tangle ct

    -- |           legsToGlue = 2
    --  ..............|
    --  (legA + 2) ---|- 0
    --  ..............|     |..............
    --  (legA + 1) ---|-----|--- (legB - 1)
    --  ..............|     |..............
    --  (legA) -------|-----|--- (legB)
    --  ..............|     |..............
    glueTangles    :: (CrossingType ct) => Int -> Dart tangle ct -> Dart tangle ct -> tangle ct

    -- |     edgesToGlue = 1                 edgesToGlue = 2                 edgesToGlue = 3
    -- ........|                       ........|                       ........|
    -- (leg+1)-|---------------3       (leg+1)-|---------------2       (leg+1)-|---------------1
    --         |  +=========+                  |  +=========+                  |  +=========+
    --  (leg)--|--|-0-\ /-3-|--2        (leg)--|--|-0-\ /-3-|--1        (leg)--|--|-0-\ /-3-|--0
    -- ........|  |    *    |                  |  |    *    |                  |  |    *    |
    -- ........|  |   / \-2-|--1       (leg-1)-|--|-1-/ \-2-|--0       (leg-1)-|--|-1-/ \   |
    -- ........|  |  1      |          ........|  +=========+                  |  |      2  |
    -- ........|  |   \-----|--0       ........|                       (leg-2)-|--|-----/   |
    -- ........|  +=========+          ........|                       ........|  +=========+
    glueToBorder   :: (CrossingType ct) => Dart tangle ct -> Int -> CrossingState ct -> Crossing tangle ct


{-# INLINE firstLeg #-}
firstLeg :: (TangleLike t) => t ct -> Dart t ct
firstLeg t = nthLeg t 0


{-# INLINE lastLeg #-}
lastLeg :: (TangleLike t) => t ct -> Dart t ct
lastLeg t = nthLeg t (-1)


{-# INLINE nextLeg #-}
nextLeg :: (TangleLike t) => Int -> Dart t ct -> Dart t ct
nextLeg n d = nthLeg (dartOwner d) (legPlace d + n)


{-# INLINE allLegOpposites #-}
allLegOpposites :: (TangleLike t) => t ct -> [Dart t ct]
allLegOpposites = map opposite . allLegs
