module Math.KnotTh.Crossings.Arbitrary.Writhe
    ( selfWrithe
    , selfWritheByThread
    , selfWritheArray
    , threadsWithLinkingNumbers
    ) where

import Data.Ix (Ix)
import Data.Array.Base (listArray, accumArray, (!))
import Data.Array.Unboxed (UArray, elems)
import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary.Arbitrary


selfWrithe :: (Knotted k c d, Ix (c ArbitraryCrossing), Ix (d ArbitraryCrossing)) => k ArbitraryCrossing -> Int
selfWrithe knot
    | hasNoCrossings knot  = 0
    | otherwise            = sum $ elems $ selfWritheArray knot


selfWritheByThread :: (Knotted k c d, Ix (d ArbitraryCrossing)) => k ArbitraryCrossing -> UArray Int Int
selfWritheByThread knot =
    let (n, tag, _) = allThreadsWithMarks knot
    in accumArray (+) 0 (1, n) $ do
        c <- allCrossings knot
        let ((a, b), w) = crossingWrithe tag c
        [(a, w) | a == b]


selfWritheArray :: (Knotted k c d, Ix (c ArbitraryCrossing), Ix (d ArbitraryCrossing)) => k ArbitraryCrossing -> UArray (c ArbitraryCrossing) Int
selfWritheArray knot =
    let (_, tag, _) = allThreadsWithMarks knot
    in listArray (crossingsRange knot) $ do
        c <- allCrossings knot
        let ((a, b), w) = crossingWrithe tag c
        return $ if a == b then w else 0


threadsWithLinkingNumbers :: (Knotted k c d, Ix (d ArbitraryCrossing))
    => k ArbitraryCrossing -> ((Int, UArray (d ArbitraryCrossing) Int, [(Int, [(d ArbitraryCrossing, d ArbitraryCrossing)])]), UArray (Int, Int) Int)

threadsWithLinkingNumbers knot =
    let ts@(n, tag, _) = allThreadsWithMarks knot

        ln = accumArray (+) 0 ((1, 1), (n, n)) $ do
            c <- allCrossings knot
            let ((a, b), w) = crossingWrithe tag c
            if a == b
                then [((a, b), w)]
                else [((a, b), w), ((b, a), w)]

    in (ts, ln)


{-# INLINE crossingWrithe #-}
crossingWrithe :: (Knotted k c d, Ix (d ArbitraryCrossing)) => UArray (d ArbitraryCrossing) Int -> c ArbitraryCrossing -> ((Int, Int), Int)
crossingWrithe t cross =
    let d0 = nthIncidentDart cross 0
        t0 = t ! d0
        t1 = t ! nextCCW d0
    in ((abs t0, abs t1), if (signum t0 == signum t1) == passOver d0 then 1 else -1)
