module Math.Topology.KnotTh.Link.Definition.Misc
    ( tangleDoubling
    ) where

import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Link.Definition.Link
import Math.Topology.KnotTh.Tangle


tangleDoubling :: (CrossingType t) => (Crossing t -> Crossing t) -> Tangle (Crossing t) -> Link (Crossing t)
tangleDoubling f t =
    let l = numberOfLegs t
        t' = mirrorTangle $ fmap f t
    in tangleToLink $ glueTangles l (firstLeg t) (firstLeg t')
