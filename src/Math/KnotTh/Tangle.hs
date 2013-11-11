module Math.KnotTh.Tangle
    ( module X
    , TangleProj
    , TangleProjCrossing
    , TangleProjDart
    , lonerProjection
    , NATangle
    , NATangleCrossing
    , NATangleDart
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


type TangleProj = Tangle ProjectionCrossing
type TangleProjCrossing = Crossing Tangle ProjectionCrossing
type TangleProjDart = Dart Tangle ProjectionCrossing


lonerProjection :: TangleProj
lonerProjection = lonerTangle projectionCrossing


type NATangle = Tangle ArbitraryCrossing
type NATangleCrossing = Crossing Tangle ArbitraryCrossing
type NATangleDart = Dart Tangle ArbitraryCrossing


lonerOverCrossingTangle :: NATangle
lonerOverCrossingTangle = lonerTangle overCrossing


lonerUnderCrossingTangle :: NATangle
lonerUnderCrossingTangle = lonerTangle underCrossing


rationalTangle :: [Int] -> NATangle
rationalTangle = flip foldl infinityTangle $ \ tangle x ->
    let g = chainTangle $ replicate (abs x) (if x >= 0 then overCrossing else underCrossing)
    in glueTangles 2 (nthLeg g 2) (nthLeg tangle 2)
