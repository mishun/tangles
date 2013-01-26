module Math.KnotTh.Invariants.Util.BruteForceMinimization
    ( bruteForceMinimumOfTangle
    , crossingsOrientationMinimum
    ) where

import Data.Function (on)
import Math.KnotTh.Tangle.NonAlternating


bruteForceMinimumOfTangle :: (Ord x) => (NonAlternatingTangle -> x) -> NonAlternatingTangle -> x
bruteForceMinimumOfTangle f = minimum . map (crossingsOrientationMinimum f) . allOrientationsOfTangle


crossingsOrientationMinimum :: (Ord x, Knotted k c d) => (k ArbitraryCrossing -> x) -> k ArbitraryCrossing -> x
crossingsOrientationMinimum f knot = on min f knot (invertCrossings knot)
