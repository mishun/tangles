module Math.Topology.KnotTh.Tangle.Definition.Transform
    ( transformTangle
    , rotateTangle
    , mirrorTangle
    , allOrientationsOfTangle
    ) where

import Text.Printf
import qualified Math.Algebra.Group.Dn as Dn
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Tangle.Definition.TangleLike
import Math.Topology.KnotTh.Tangle.Definition.Tangle


transformTangle :: (CrossingType ct) => Dn.Dn -> Tangle ct -> Tangle ct
transformTangle g tangle
    | l /= Dn.pointsUnderGroup g                   =
        error $ printf "transformTangle: order conflict: %i legs, %i order of group" l (Dn.pointsUnderGroup g)
    | not (Dn.reflection g) && Dn.rotation g == 0  =
        tangle
    | otherwise                                    =
        implode (numberOfFreeLoops tangle, border, map crossing $ allCrossings tangle)
    where
        l = numberOfLegs tangle

        pair d | isLeg d    = (0, Dn.permute g $ legPlace d)
               | otherwise  =
                   let c = incidentCrossing d
                   in (crossingIndex c, if Dn.reflection g then 3 - dartPlace d else dartPlace d)

        crossing c | Dn.reflection g  = (reverse $ map pair $ adjacentDarts c, mirrorReversingDartsOrder $ crossingState c)
                   | otherwise        = (map pair $ adjacentDarts c, crossingState c)

        border | Dn.reflection g  = head rotated : reverse (tail rotated)
               | otherwise        = rotated
            where
                rotated =
                    let (pre, post) = splitAt (l - Dn.rotation g) $ map (pair . opposite) $ allLegs tangle
                    in post ++ pre


rotateTangle :: (CrossingType ct) => Int -> Tangle ct -> Tangle ct
rotateTangle rot tangle =
    case numberOfLegs tangle of
        0 -> tangle
        l -> transformTangle (Dn.fromRotation l rot) tangle


mirrorTangle :: (CrossingType ct) => Tangle ct -> Tangle ct
mirrorTangle tangle =
    let l = numberOfLegs tangle
    in transformTangle (Dn.fromReflectionRotation l (True, 0)) tangle


allOrientationsOfTangle :: (CrossingType ct) => Tangle ct -> [Tangle ct]
allOrientationsOfTangle tangle = do
    let l = numberOfLegs tangle
    if l == 0
        then return $! tangle
        else do
            g <- [ Dn.fromReflectionRotation l (refl, rot) | rot <- [0 .. l - 1], refl <- [False, True] ]
            return $! transformTangle g tangle
