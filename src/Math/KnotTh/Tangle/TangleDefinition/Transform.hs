module Math.KnotTh.Tangle.TangleDefinition.Transform
    ( transformTangle
    , rotateTangle
    , mirrorTangle
    , allOrientationsOfTangle
    ) where

import Text.Printf
import Math.Algebra.Group.Dn (Dn, pointsUnderGroup, reflection, rotation, permute, fromRotation, fromReflectionRotation)
import Math.KnotTh.Tangle.TangleDefinition.Class
import Math.KnotTh.Tangle.TangleDefinition.Tangle


transformTangle :: (CrossingType ct) => Dn -> Tangle ct -> Tangle ct
transformTangle g tangle
    | l /= pointsUnderGroup g                   = error $ printf "transformTangle: order conflict: %i legs, %i order of group" l (pointsUnderGroup g)
    | reflection g == False && rotation g == 0  = tangle
    | otherwise                                 = implode (numberOfFreeLoops tangle, border, map crossing $ allCrossings tangle)
    where
        l = numberOfLegs tangle

        pair d
            | isLeg d    = (0, permute g $ legPlace d)
            | otherwise  =
                let c = incidentCrossing d
                in (crossingIndex c, if reflection g then 3 - dartPlace d else dartPlace d)

        crossing c
            | reflection g  = (reverse $ map pair $ adjacentDarts c, mirrorReversingDartsOrder $ crossingState c)
            | otherwise     = (map pair $ adjacentDarts c, crossingState c)

        border
            | reflection g  = head rotated : reverse (tail rotated)
            | otherwise     = rotated
            where
                rotated =
                    let (pre, post) = splitAt (l - rotation g) $ map (pair . opposite) $ allLegs tangle
                    in post ++ pre


rotateTangle :: (CrossingType ct) => Int -> Tangle ct -> Tangle ct
rotateTangle rot tangle =
    case numberOfLegs tangle of
        0 -> tangle
        l -> transformTangle (fromRotation l rot) tangle


mirrorTangle :: (CrossingType ct) => Tangle ct -> Tangle ct
mirrorTangle tangle =
    let l = numberOfLegs tangle
    in transformTangle (fromReflectionRotation l (True, 0)) tangle


allOrientationsOfTangle :: (CrossingType ct) => Tangle ct -> [Tangle ct]
allOrientationsOfTangle tangle = do
    let l = numberOfLegs tangle
    if l == 0
        then return $! tangle
        else do
            g <- [ fromReflectionRotation l (refl, rot) | rot <- [0 .. l - 1], refl <- [False, True] ]
            return $! transformTangle g tangle
