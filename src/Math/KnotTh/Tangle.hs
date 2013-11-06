module Math.KnotTh.Tangle
    ( module X
    , TangleProjection
    , lonerProjection
    , NonAlternatingTangle
    , NonAlternatingCrossing
    , NonAlternatingDart
    , lonerOverCrossingTangle
    , lonerUnderCrossingTangle
    , rationalTangle
    ) where

import Math.KnotTh.Knotted as X
import Math.KnotTh.Tangle.Definition.TangleLike as X
import Math.KnotTh.Tangle.Definition.Tangle as X
import Math.KnotTh.Tangle.Definition.Braid as X
import Math.KnotTh.Tangle.Definition.Transform as X
import Math.KnotTh.Tangle.Definition.Misc as X
import Math.KnotTh.Tangle.Definition.CascadeCode as X
import Math.KnotTh.Crossings.Projection as X
import Math.KnotTh.Crossings.Arbitrary as X


type TangleProjection = Tangle ProjectionCrossing


lonerProjection :: TangleProjection
lonerProjection = lonerTangle projectionCrossing


type NonAlternatingTangle = Tangle ArbitraryCrossing

type NonAlternatingCrossing = Crossing Tangle ArbitraryCrossing

type NonAlternatingDart = Dart Tangle ArbitraryCrossing


lonerOverCrossingTangle :: NonAlternatingTangle
lonerOverCrossingTangle = lonerTangle overCrossing


lonerUnderCrossingTangle :: NonAlternatingTangle
lonerUnderCrossingTangle = lonerTangle underCrossing


rationalTangle :: [Int] -> NonAlternatingTangle
rationalTangle = flip foldl infinityTangle $ \ tangle x ->
    let g = chainTangle $ replicate (abs x) (if x >= 0 then overCrossing else underCrossing)
    in glueTangles 2 (nthLeg g 2) (nthLeg tangle 2)
