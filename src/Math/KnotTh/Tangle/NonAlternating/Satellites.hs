module Math.KnotTh.Tangle.NonAlternating.Satellites
    ( twistedDoubleSatellite
    , twistedTripleSatellite
    ) where

import Data.Array.Base ((!))
import Math.KnotTh.Tangle
import Math.KnotTh.Tangle.TensorSubst


twistedDoubleSatellite :: NonAlternatingTangle -> NonAlternatingTangle
twistedDoubleSatellite = twistedNSatellite 2


twistedTripleSatellite :: NonAlternatingTangle -> NonAlternatingTangle
twistedTripleSatellite = twistedNSatellite 3


twistedNSatellite :: Int -> NonAlternatingTangle -> NonAlternatingTangle
twistedNSatellite n tangle
    | n < 0      = error "twistedNSattelite: negative order"
    | n == 0     = emptyTangle
    | n == 1     = tangle
    | otherwise  = tensorSubst n wrap tangle
    where
        w = selfWritheArray tangle

        wrap c
            | wc == 0    = cross
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
