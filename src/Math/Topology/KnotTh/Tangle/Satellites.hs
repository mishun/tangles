module Math.Topology.KnotTh.Tangle.Satellites
    ( twistedDoubleSatellite
    , twistedTripleSatellite
    ) where

import Data.Array.IArray ((!))
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Tangle.TensorSubst


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

        wrap c | wc == 0    = cross
               | otherwise  =
                   let r | wc > 0     = underCrossing
                         | otherwise  = overCrossing

                       braid =
                           let half = reversingBraidTangle n r
                           in half |=| half
                   in glueTangles n (nthLeg braid n) (nthLeg cross $ n - 1)
            where
                wc = w ! c
                s = crossingState c
                cross = gridTangle (n, n) (const s)
