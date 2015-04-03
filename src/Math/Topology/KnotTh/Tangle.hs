module Math.Topology.KnotTh.Tangle
    ( module Math.Topology.KnotTh.Knotted
    , module Math.Topology.KnotTh.Crossings.Projection
    , module Math.Topology.KnotTh.Crossings.Diagram
    , module Math.Topology.KnotTh.Tangle.TangleLike
    , module Math.Topology.KnotTh.Tangle.Tangle
    , module Math.Topology.KnotTh.Tangle.Braid
    , module Math.Topology.KnotTh.Tangle.Misc
    , module Math.Topology.KnotTh.Tangle.CascadeCode
    , lonerProjection
    , lonerOverCrossing
    , lonerUnderCrossing
    , rationalTangle
    ) where

import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Crossings.Projection
import Math.Topology.KnotTh.Crossings.Diagram
import Math.Topology.KnotTh.Tangle.TangleLike
import Math.Topology.KnotTh.Tangle.Tangle
import Math.Topology.KnotTh.Tangle.Braid
import Math.Topology.KnotTh.Tangle.Misc
import Math.Topology.KnotTh.Tangle.CascadeCode


lonerProjection :: TangleProjection
lonerProjection = lonerTangle projectionCrossing


lonerOverCrossing, lonerUnderCrossing :: TangleDiagram
lonerOverCrossing = lonerTangle overCrossing
lonerUnderCrossing = lonerTangle underCrossing


rationalTangle :: [Int] -> TangleDiagram
rationalTangle = flip foldl infinityTangle $ \ tangle x ->
    let g = chainTangle $ replicate (abs x) (if x >= 0 then overCrossing else underCrossing)
    in glueTangles 2 (nthLeg g 2) (nthLeg tangle 2)
