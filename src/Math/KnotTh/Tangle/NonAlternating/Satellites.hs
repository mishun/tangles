module Math.KnotTh.Tangle.NonAlternating.Satellites
    ( twistedDouble
    , twistedTriple
    ) where

import Data.Array.Base ((!))
import Math.KnotTh.Tangle.NonAlternating
import Math.KnotTh.Tangle.Table
import Math.KnotTh.Tangle.TensorSubst


twistedDouble :: NonAlternatingTangle -> NonAlternatingTangle
twistedDouble = twistedNple 2


twistedTriple :: NonAlternatingTangle -> NonAlternatingTangle
twistedTriple = twistedNple 3


twistedNple :: Int -> NonAlternatingTangle -> NonAlternatingTangle
twistedNple n tangle = tensorSubst n wrap tangle
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
                wc = w ! crossingIndex c
                s = crossingState c
                cross = gridTangle (n, n) (const s)
