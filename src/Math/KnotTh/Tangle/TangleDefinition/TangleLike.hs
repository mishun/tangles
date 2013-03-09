module Math.KnotTh.Tangle.TangleDefinition.TangleLike
    ( TangleLike(..)
    , firstLeg
    , lastLeg
    , nextLeg
    , allLegOpposites
    ) where

import Math.KnotTh.Knotted


class (Knotted tangle cross dart) => TangleLike tangle cross dart | tangle -> cross, cross -> dart, dart -> tangle where
    numberOfLegs   :: tangle ct -> Int
    allLegs        :: tangle ct -> [dart ct]
    nthLeg         :: tangle ct -> Int -> dart ct

    isLeg          :: dart ct -> Bool
    legPlace       :: dart ct -> Int

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
    glueTangles    :: (CrossingType ct) => Int -> dart ct -> dart ct -> tangle ct

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
    glueToBorder   :: (CrossingType ct) => dart ct -> Int -> CrossingState ct -> cross ct


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
