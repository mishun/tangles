module Math.Topology.KnotTh.Link.Definition.Misc
    ( tangleDoubling
    ) where

import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Link.Definition.Link
import Math.Topology.KnotTh.Tangle


tangleDoubling :: (CrossingType ct) => (CrossingState ct -> CrossingState ct) -> Tangle ct -> Link ct
tangleDoubling f t =
    let l = numberOfLegs t
        t' = mirrorTangle $ mapCrossings f t
    in tangleToLink $ glueTangles l (firstLeg t) (firstLeg t')
