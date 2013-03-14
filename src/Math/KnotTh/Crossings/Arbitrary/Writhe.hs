module Math.KnotTh.Crossings.Arbitrary.Writhe
    ( selfWrithe
    , selfWritheArray
    ) where

import Data.Ix (Ix)
import Data.Array.Base (listArray, (!))
import Data.Array.Unboxed (UArray, elems)
import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary.Arbitrary


selfWrithe :: (Knotted k c d, Ix (c ArbitraryCrossing), Ix (d ArbitraryCrossing)) => k ArbitraryCrossing -> Int
selfWrithe knot | numberOfCrossings knot == 0  = 0
                | otherwise                    = sum $ elems $ selfWritheArray knot


selfWritheArray :: (Knotted k c d, Ix (c ArbitraryCrossing), Ix (d ArbitraryCrossing)) => k ArbitraryCrossing -> UArray (c ArbitraryCrossing) Int
selfWritheArray knot =
    let (_, t, _) = allThreadsWithMarks knot
        writhe !cross
            | t0 == t1     = s
            | t0 == (-t1)  = -s
            | otherwise    = 0
            where
                d0 = nthIncidentDart cross 0
                t0 = t ! d0
                t1 = t ! nextCCW d0
                s | passOver d0  = 1
                  | otherwise    = -1
    in listArray (crossingsRange knot) $ map writhe $ allCrossings knot
