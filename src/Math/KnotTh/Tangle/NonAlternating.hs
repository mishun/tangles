module Math.KnotTh.Tangle.NonAlternating
    ( module Math.KnotTh.Crossings.Arbitrary
    , module Math.KnotTh.Tangle
    , NonAlternatingTangle
    , NonAlternatingCrossing
    , NonAlternatingDart
    , lonerOverCrossingTangle
    , lonerUnderCrossingTangle
    , altTriangleTangle
    , naTriangleTangle
    , rationalTangle
    ) where

import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Tangle
import Math.KnotTh.Tangle.CascadeCode


type NonAlternatingTangle = Tangle ArbitraryCrossing

type NonAlternatingCrossing = Crossing Tangle ArbitraryCrossing

type NonAlternatingDart = Dart Tangle ArbitraryCrossing


lonerOverCrossingTangle :: NonAlternatingTangle
lonerOverCrossingTangle = lonerTangle overCrossing


lonerUnderCrossingTangle :: NonAlternatingTangle
lonerUnderCrossingTangle = lonerTangle underCrossing


altTriangleTangle :: NonAlternatingTangle
altTriangleTangle = decodeCascadeCode [(MU, 0), (XO, 1)]


naTriangleTangle :: NonAlternatingTangle
naTriangleTangle = decodeCascadeCode [(MU, 0), (XU, 1)]


rationalTangle :: [Int] -> NonAlternatingTangle
rationalTangle = flip foldl infinityTangle $ \ tangle x ->
    let g = chainTangle $ replicate (abs x) (if x >= 0 then overCrossing else underCrossing)
    in glueTangles 2 (nthLeg g 2) (nthLeg tangle 2)
