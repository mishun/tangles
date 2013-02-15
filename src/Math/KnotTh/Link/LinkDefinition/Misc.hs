module Math.KnotTh.Link.LinkDefinition.Misc
    ( toTangle
    , fromTangle
    , tangleDoubling
    ) where

import Text.Printf
import Math.KnotTh.Knotted
import Math.KnotTh.Link.LinkDefinition.Link
import qualified Math.KnotTh.Tangle as T


toTangle :: (CrossingType ct) => Link ct -> T.Tangle ct
toTangle link =
    let (loops, cross) = explode link
    in T.implode (loops, [], cross)


fromTangle :: (CrossingType ct) => T.Tangle ct -> Link ct
fromTangle tangle
    | l /= 0     = error $ printf "fromTangle: tangle exptected to have 0 legs, but %i present" l
    | otherwise  =
        let (loops, _, cross) = T.explode tangle
        in implode (loops, cross)
    where
        l = T.numberOfLegs tangle


tangleDoubling :: (CrossingType ct) => (CrossingState ct -> CrossingState ct) -> T.Tangle ct -> Link ct
tangleDoubling f t =
    let l = T.numberOfLegs t
        t' = T.mirrorTangle $ mapCrossings f t
    in fromTangle $ T.glueTangles l (T.firstLeg t) (T.firstLeg t')
