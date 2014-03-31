module Math.Topology.KnotTh.Tangle
    ( module X
    , lonerProjection
    , lonerOverCrossing
    , lonerUnderCrossing
    , rationalTangle
    ) where

import Math.Topology.KnotTh.Knotted as X
import Math.Topology.KnotTh.Crossings.Projection as X
import Math.Topology.KnotTh.Crossings.Diagram as X
import Math.Topology.KnotTh.Tangle.TangleLike as X
import Math.Topology.KnotTh.Tangle.Tangle as X
import Math.Topology.KnotTh.Tangle.Braid as X
import Math.Topology.KnotTh.Tangle.Misc as X
import Math.Topology.KnotTh.Tangle.CascadeCode as X


lonerProjection :: TangleProjection
lonerProjection = lonerTangle projectionCrossing


lonerOverCrossing, lonerUnderCrossing :: TangleDiagram
lonerOverCrossing = lonerTangle overCrossing
lonerUnderCrossing = lonerTangle underCrossing


rationalTangle :: [Int] -> TangleDiagram
rationalTangle = flip foldl infinityTangle $ \ tangle x ->
    let g = chainTangle $ replicate (abs x) (if x >= 0 then overCrossing else underCrossing)
    in glueTangles 2 (nthLeg g 2) (nthLeg tangle 2)
