module Math.KnotTh.Crossings.Arbitrary.Alternating
    ( alternatingDefect
    , isAlternating
    , isNonAlternating
    ) where

import Math.KnotTh.Knotted
import Math.KnotTh.Crossings.Arbitrary.Arbitrary


alternatingDefect :: (Knotted k c d) => k ArbitraryCrossing -> Int
alternatingDefect =
    let defect (!a, !b)
            | isEndpoint a || isEndpoint b  = 0
            | passOver a == passOver b      = 1
            | otherwise                     = 0
    in sum . map defect . allEdges


isAlternating :: (Knotted k c d) => k ArbitraryCrossing -> Bool
isAlternating = (== 0) . alternatingDefect


isNonAlternating :: (Knotted k c d) => k ArbitraryCrossing -> Bool
isNonAlternating = not . isAlternating
