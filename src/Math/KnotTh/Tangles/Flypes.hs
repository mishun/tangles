{-# LANGUAGE UnboxedTuples #-}
module Math.KnotTh.Tangles.Flypes
	( minimumFlypeCode
	, additionalFlypeSymmetry
	) where

import Data.Array.Base (unsafeWrite)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STUArray, newArray)
import Data.Array.Unsafe (unsafeFreeze)
import Control.Monad.ST (ST, runST)
import Math.Algebra.RotationDirection
import Math.Algebra.Group.Dn (Dn, fromReflectionRotation)
import Math.KnotTh.Crossings.Projection
import Math.KnotTh.Crossings.SubTangle
import Math.KnotTh.Tangles


flypeCodeLeg :: Dart (SubTangleCrossing ProjectionCrossing) -> RotationDirection -> UArray Int Int
flypeCodeLeg leg initialDirection
	| isDart leg  = error "flypeCodeLeg: leg expected"
	| otherwise   = runST $ do
		let tangle = dartTangle leg
		let n = numberOfCrossings tangle
		code <- newArray (0, 2 * n - 1) 0 :: ST s (STUArray s Int Int)

		let	{-# INLINE go #-}
			go !i !d !dir
				| i >= n || isLeg d                   = return ()
				| isLonerInside (incidentCrossing d)  = go i (opposite $ continuation d) (oppositeDirection dir)
				| otherwise                           = do
					case crossingCode dir d of
						(# be, le #) -> do
							unsafeWrite code (2 * i) be
							unsafeWrite code (2 * i + 1) le
					go (i + 1) (opposite $ nextDir dir d) dir

		go 0 (opposite leg) initialDirection
		unsafeFreeze code


minimumFlypeCode :: Tangle (SubTangleCrossing ProjectionCrossing) -> UArray Int Int
minimumFlypeCode tangle
	| numberOfLegs tangle /= 4          = error "minimumFlypeCode: tangle with 4 legs expected"
	| (a == b) && (c == d) && (a /= c)  = minimum $ map (\ (leg, dir) -> flypeCodeLeg leg dir) [(l0, cw), (l1, ccw), (l2, cw), (l3, ccw)]
	| (b == c) && (a == d) && (a /= b)  = minimum $ map (\ (leg, dir) -> flypeCodeLeg leg dir) [(l0, ccw), (l1, cw), (l2, ccw), (l3, cw)]
	| otherwise                         = error "minimumFlypeCode: direct sum expected"
	where
		[l0, l1, l2, l3] = allLegs tangle
		[a, b, c, d] = map adjacentCrossing $ allLegs tangle


additionalFlypeSymmetry :: Tangle (SubTangleCrossing ProjectionCrossing) -> Maybe Dn
additionalFlypeSymmetry tangle
	| numberOfLegs tangle /= 4                 = error "additionalFlypeSymmetry: tangle with 4 legs expected"
	| x == flypeCodeLeg (nthLeg tangle 2) dir  = Just $! fromReflectionRotation 4 (False, 2)
	| x == flypeCodeLeg (nthLeg tangle 1) rev  = Just $! fromReflectionRotation 4 (True, 3)
	| x == flypeCodeLeg (nthLeg tangle 3) rev  = Just $! fromReflectionRotation 4 (True, 1)
	| otherwise                                = Nothing
	where
		x = flypeCodeLeg (nthLeg tangle 0) dir

		dir
			| (a == b) && (c == d) && (a /= c)  = cw
			| (b == c) && (a == d) && (a /= b)  = ccw
			| otherwise                         = error "additionalFlypeSymmetry: direct sum expected"
			where
				[a, b, c, d] = map adjacentCrossing $ allLegs tangle

		rev = oppositeDirection dir
