module Math.Topology.KnotTh.Invariants.LinkingNumber
    ( linkingNumbersInvariant
    ) where

import Data.Ix (Ix)
import Data.List (sort)
import Data.Array.IArray ((!))
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Crossings.Arbitrary


linkingNumbersInvariant :: (Knotted k, Ix (Dart k ArbitraryCrossing)) => k ArbitraryCrossing -> [Int]
linkingNumbersInvariant knot = sort $ do
    let ((n, _, _), ln) = threadsWithLinkingNumbers knot
    i <- [1 .. n]
    j <- [1 .. i - 1]
    return $! abs $! ln ! (i, j)
