module Math.Topology.KnotTh.Tangle.Misc
    ( mirrorTangle
    , transformTangle
    , allOrientationsOfTangle
    , gridTangle
    , chainTangle
    , twistedDoubleSatellite
    , twistedTripleSatellite
    ) where

import Data.Array.IArray ((!))
import Text.Printf
import qualified Math.Algebra.Group.Dn as Dn
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Crossings.Diagram
import Math.Topology.KnotTh.Tangle.TangleLike
import Math.Topology.KnotTh.Tangle.Tangle
import Math.Topology.KnotTh.Tangle.Braid


transformTangle :: (Crossing a, TangleLike t) => Dn.Dn -> t a -> t a
transformTangle g tangle
    | l /= l'          = error $ printf "transformTangle: order conflict: %i legs, %i order of group" l l'
    | Dn.reflection g  = mirrorTangle $ rotateTangle r tangle
    | otherwise        = rotateTangle r tangle
    where
        l = numberOfLegs tangle
        l' = Dn.pointsUnderGroup g
        r = Dn.rotation g


allOrientationsOfTangle :: (Crossing a, TangleLike t) => t a -> [t a]
allOrientationsOfTangle tangle = do
    t <- let l = numberOfLegs tangle
         in if l == 0
             then [tangle]
             else map (`rotateTangle` tangle) [0 .. l]
    [t, mirrorTangle t]


gridTangle :: (Int, Int) -> ((Int, Int) -> a) -> Tangle a
gridTangle (n, m) f
    | n < 0      = error $ printf "gridTangle: first dimension %i is negative" n
    | m < 0      = error $ printf "gridTangle: second dimension %i is negative" m
    | otherwise  =
        let border = ([1 .. n] `zip` repeat 0) ++ (map (\ i -> n * i) [1 .. m] `zip` repeat 1)
                ++ (map (\ i -> n * m + 1 - i) [1 .. n] `zip` repeat 2)
                ++ (map (\ i -> (m - i) * n + 1) [1 .. m] `zip` repeat 3)

            body = do
                j <- [1 .. m]
                i <- [1 .. n]
                return (
                    [ if j > 1 then (n * (j - 2) + i    , 2) else (0, i - 1            )
                    , if i < n then (n * (j - 1) + i + 1, 3) else (0, j + n - 1        )
                    , if j < m then (n * j + i          , 0) else (0, 2 * n + m - i    )
                    , if i > 1 then (n * (j - 1) + i - 1, 1) else (0, 2 * m + 2 * n - j)
                    ], f (i, j))
        in implode (0, border, body)


chainTangle :: [a] -> Tangle a
chainTangle [] = zeroTangle
chainTangle list =
    let n = length list
    in implode
        ( 0
        , [(1, 0), (1, 1), (n, 2), (n, 3)]
        , map (\ (i, s) ->
            (   [ if i > 1 then (i - 1, 3) else (0, 0)
                , if i > 1 then (i - 1, 2) else (0, 1)
                , if i < n then (i + 1, 1) else (0, 2)
                , if i < n then (i + 1, 0) else (0, 3)
                ]
            , s
            )) ([1 .. n] `zip` list)
        )


twistedDoubleSatellite :: TangleDiagram -> TangleDiagram
twistedDoubleSatellite = twistedNSatellite 2


twistedTripleSatellite :: TangleDiagram -> TangleDiagram
twistedTripleSatellite = twistedNSatellite 3


twistedNSatellite :: Int -> TangleDiagram -> TangleDiagram
twistedNSatellite n tangle
    | n < 0      = error "twistedNSattelite: negative order"
    | n == 0     = emptyTangle
    | n == 1     = tangle
    | otherwise  = tensorSubst n wrap tangle
    where
        w = selfWritheArray tangle

        wrap v | wc == 0    = cross
               | otherwise  =
                   let r | wc > 0     = underCrossing
                         | otherwise  = overCrossing

                       braid =
                           let half = reversingBraidTangle n r
                           in half |=| half
                   in glueTangles n (nthLeg braid n) (nthLeg cross $ n - 1)
            where
                wc = w ! v
                s = vertexCrossing v
                cross = gridTangle (n, n) (const s)
