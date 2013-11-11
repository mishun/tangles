module Math.Topology.KnotTh.Link.Definition.Misc
    ( toTangle
    , fromTangle
    , tangleDoubling
    ) where

import Text.Printf
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Link.Definition.Link
import Math.Topology.KnotTh.Tangle (Tangle, numberOfLegs, firstLeg, glueTangles, mirrorTangle)


toTangle :: (CrossingType ct) => Link ct -> Tangle ct
toTangle link =
    let (loops, cross) = explode link
    in implode (loops, [], cross)


fromTangle :: (CrossingType ct) => Tangle ct -> Link ct
fromTangle tangle
    | l /= 0     = error $ printf "fromTangle: tangle exptected to have 0 legs, but %i present" l
    | otherwise  =
        let (loops, _, cross) = explode tangle
        in implode (loops, cross)
    where
        l = numberOfLegs tangle


tangleDoubling :: (CrossingType ct) => (CrossingState ct -> CrossingState ct) -> Tangle ct -> Link ct
tangleDoubling f t =
    let l = numberOfLegs t
        t' = mirrorTangle $ mapCrossings f t
    in fromTangle $ glueTangles l (firstLeg t) (firstLeg t')
