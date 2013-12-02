module Math.Topology.KnotTh.Tangle.Definition.TangleLike
    ( TangleLike(..)
    ) where

import Math.Topology.KnotTh.Knotted


class (Knotted t, PlanarAlgebra t) => TangleLike t where
    emptyTangle :: t a
    emptyTangle = emptyKnotted

    -- | 2--1
    --   3--0
    zeroTangle     :: t a

    -- | 2   1
    --   |   |
    --   3   0
    infinityTangle :: t a

    -- | +-------+
    --   |   ^   |
    --   |   |   |
    --   |   |   |
    --   +-------+
    identityTangle :: t a

    lonerTangle :: a -> t a

    rotateTangle     :: Int -> t a -> t a
    mirrorTangleWith :: (a -> a) -> t a -> t a
    mirrorTangle     :: (Crossing a) => t a -> t a

    mirrorTangle = mirrorTangleWith mirrorCrossing

    -- |           legsToGlue = 2
    --  ..............|
    --  (legA + 2) ---|- 0
    --  ..............|     |..............
    --  (legA + 1) ---|-----|--- (legB - 1)
    --  ..............|     |..............
    --  (legA) -------|-----|--- (legB)
    --  ..............|     |..............
    glueTangles :: Int -> Dart t a -> Dart t a -> t a

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
    glueToBorder :: Dart t a -> Int -> a -> Vertex t a

    tensorSubst :: Int -> (Vertex t a -> t b) -> t a -> t b
