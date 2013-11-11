module Math.KnotTh.Invariants.Util.BruteForceMinimization
    ( bruteForceMinimumOfTangle
    , crossingsOrientationMinimum
    ) where

import Data.Function (on)
import Math.KnotTh.Tangle


bruteForceMinimumOfTangle :: (Ord x) => (NATangle -> x) -> NATangle -> x
bruteForceMinimumOfTangle f = minimum . map (crossingsOrientationMinimum f) . allOrientationsOfTangle


crossingsOrientationMinimum :: (Ord x, Knotted k) => (k ArbitraryCrossing -> x) -> k ArbitraryCrossing -> x
crossingsOrientationMinimum f knot = on min f knot (invertCrossings knot)
