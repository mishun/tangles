module Math.KnotTh.Invariants.Util.BruteForceMinimization
	( bruteForceMinimumOfTangle
	, crossingsOrientationMinimum
	) where

import Data.Function (on)
import Math.Algebra.Group.Dn (fromReflectionRotation)
import Math.KnotTh.Tangle.NonAlternating


bruteForceMinimumOfTangle :: (Ord x) => (NonAlternatingTangle -> x) -> NonAlternatingTangle -> x
bruteForceMinimumOfTangle f tangle = minimum $ do
	let l = numberOfLegs tangle
	g <- [ fromReflectionRotation l (refl, rot) | rot <- [0 .. l - 1], refl <- [False, True] ]
	return $! crossingsOrientationMinimum f $! transformTangle g tangle


crossingsOrientationMinimum :: (Ord x, Knotted k c d) => (k ArbitraryCrossing -> x) -> k ArbitraryCrossing -> x
crossingsOrientationMinimum f knot = on min f knot (invertCrossings knot)
