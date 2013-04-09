module Math.KnotTh.Invariants.LinkingNumber
    ( linkingNumbersSet
    ) where

import Data.Ix (Ix)
import Data.List (sort)
import Data.Array.Base ((!))
import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary


linkingNumbersSet :: (Knotted k c d, Ix (d ArbitraryCrossing)) => k ArbitraryCrossing -> [Int]
linkingNumbersSet knot = sort $ do
    let ((n, _, _), ln) = threadsWithLinkingNumbers knot
    i <- [1 .. n]
    j <- [1 .. i - 1]
    return $! abs $! ln ! (i, j)
