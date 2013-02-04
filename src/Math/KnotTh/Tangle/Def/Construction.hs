module Math.KnotTh.Tangle.Def.Construction
    ( lonerTangle
    , emptyTangle
    , zeroTangle
    , infinityTangle
    , transformTangle
    , rotateTangle
    , allOrientationsOfTangle
    , fromLink
    ) where

import Text.Printf
import Math.Algebra.Group.Dn (Dn, pointsUnderGroup, reflection, rotation, permute, fromRotation, fromReflectionRotation)
import Math.Algebra.Group.D4 ((<*>), ec)
import Math.KnotTh.Tangle.Def.Tangle
import qualified Math.KnotTh.Link as L


lonerTangle :: (CrossingType ct) => CrossingState ct -> Tangle ct
lonerTangle !cr = implode
    ( 0
    , [(1, 0), (1, 1), (1, 2), (1, 3)]
    , [([(0, 0), (0, 1), (0, 2), (0, 3)], cr)]
    )


emptyTangle :: (CrossingType ct) => Tangle ct
emptyTangle = implode (0, [], [])


zeroTangle :: (CrossingType ct) => Tangle ct
zeroTangle = implode (0, [(0, 3), (0, 2), (0, 1), (0, 0)], [])


infinityTangle :: (CrossingType ct) => Tangle ct
infinityTangle = implode (0, [(0, 1), (0, 0), (0, 3), (0, 2)], [])


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
            | reflection g  = (reverse $ map pair $ adjacentDarts c, mapOrientation (ec <*>) $ crossingState c)
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


allOrientationsOfTangle :: (CrossingType ct) => Tangle ct -> [Tangle ct]
allOrientationsOfTangle tangle = do
    let l = numberOfLegs tangle
    if l == 0
        then return $! tangle
        else do
            g <- [ fromReflectionRotation l (refl, rot) | rot <- [0 .. l - 1], refl <- [False, True] ]
            return $! transformTangle g tangle


fromLink :: (CrossingType ct) => L.Link ct -> Tangle ct
fromLink link =
    let (loops, cross) = L.explode link
    in implode (loops, [], cross)
