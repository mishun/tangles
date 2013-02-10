module Math.KnotTh.Tangle.Table
    ( emptyTangle
    , identityTangle
    , zeroTangle
    , infinityTangle
    , lonerOverCrossingTangle
    , lonerUnderCrossingTangle
    , altTriangleTangle
    , naTriangleTangle
    , groupTangle
    ) where

import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.CascadeCode


lonerOverCrossingTangle :: NonAlternatingTangle
lonerOverCrossingTangle = lonerTangle overCrossing


lonerUnderCrossingTangle :: NonAlternatingTangle
lonerUnderCrossingTangle = lonerTangle underCrossing


altTriangleTangle :: NonAlternatingTangle
altTriangleTangle = decodeCascadeCode [(MU, 0), (XO, 1)]


naTriangleTangle :: NonAlternatingTangle
naTriangleTangle = decodeCascadeCode [(MU, 0), (XU, 1)]


groupTangle :: Int -> NonAlternatingTangle
groupTangle 0 = zeroTangle
groupTangle n =
    let cr | n > 0      = overCrossing
           | otherwise  = underCrossing
    in implode
        ( 0
        , [(1, 0), (1, 1), (abs n, 2), (abs n, 3)]
        , flip map [1 .. abs n] $ \ i ->
            let d0 | i > 1      = (i - 1, 3)
                   | otherwise  = (0, 0)
                d1 | i > 1      = (i - 1, 2)
                   | otherwise  = (0, 1)
                d2 | i < abs n  = (i + 1, 1)
                   | otherwise  = (0, 2)
                d3 | i < abs n  = (i + 1, 0)
                   | otherwise  = (0, 3)
            in ([d0, d1, d2, d3], cr)
        )
