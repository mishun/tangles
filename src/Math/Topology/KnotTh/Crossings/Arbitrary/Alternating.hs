module Math.Topology.KnotTh.Crossings.Arbitrary.Alternating
    ( alternatingDefect
    , isAlternating
    , isNonAlternating
    ) where

import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Crossings.Arbitrary.Arbitrary


alternatingDefect :: (Knotted k) => k ArbitraryCrossing -> Int
alternatingDefect =
    let defect (!a, !b)
            | isEndpoint a || isEndpoint b  = 0
            | passOver a == passOver b      = 1
            | otherwise                     = 0
    in sum . map defect . allEdges


isAlternating :: (Knotted k) => k ArbitraryCrossing -> Bool
isAlternating = (== 0) . alternatingDefect


isNonAlternating :: (Knotted k) => k ArbitraryCrossing -> Bool
isNonAlternating = not . isAlternating
