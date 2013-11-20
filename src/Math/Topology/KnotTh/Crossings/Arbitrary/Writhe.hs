module Math.Topology.KnotTh.Crossings.Arbitrary.Writhe
    ( selfWrithe
    , selfWritheByThread
    , selfWritheArray
    , threadsWithLinkingNumbers
    ) where

import Data.Array.IArray (listArray, accumArray, (!), elems)
import Data.Array.Unboxed (UArray)
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Crossings.Arbitrary.Arbitrary


selfWrithe :: (Knotted k) => k ArbitraryCrossing -> Int
selfWrithe knot | hasVertices knot  = sum $ elems $ selfWritheArray knot
                | otherwise         = 0


selfWritheByThread :: (Knotted k) => k ArbitraryCrossing -> UArray Int Int
selfWritheByThread knot =
    let (n, tag, _) = allThreadsWithMarks knot
    in accumArray (+) 0 (1, n) $ do
        c <- allVertices knot
        let ((a, b), w) = crossingWrithe tag c
        [(a, w) | a == b]


selfWritheArray :: (Knotted k) => k ArbitraryCrossing -> UArray (Vertex k ArbitraryCrossing) Int
selfWritheArray knot =
    let (_, tag, _) = allThreadsWithMarks knot
    in listArray (verticesRange knot) $ do
        c <- allVertices knot
        let ((a, b), w) = crossingWrithe tag c
        return $ if a == b then w else 0


threadsWithLinkingNumbers :: (Knotted k)
    => k ArbitraryCrossing
        -> ((Int, UArray (Dart k ArbitraryCrossing) Int, [(Int, [(Dart k ArbitraryCrossing, Dart k ArbitraryCrossing)])]), UArray (Int, Int) Int)

threadsWithLinkingNumbers knot =
    let ts@(n, tag, _) = allThreadsWithMarks knot

        ln = accumArray (+) 0 ((1, 1), (n, n)) $ do
            c <- allVertices knot
            let ((a, b), w) = crossingWrithe tag c
            if a == b
                then [((a, b), w)]
                else [((a, b), w), ((b, a), w)]

    in (ts, ln)


{-# INLINE crossingWrithe #-}
crossingWrithe :: (Knotted k)
    => UArray (Dart k ArbitraryCrossing) Int -> Vertex k ArbitraryCrossing -> ((Int, Int), Int)

crossingWrithe t cross =
    let d0 = nthOutcomingDart cross 0
        t0 = t ! d0
        t1 = t ! nextCCW d0
    in ((abs t0, abs t1), if (signum t0 == signum t1) == passOver d0 then 1 else -1)
