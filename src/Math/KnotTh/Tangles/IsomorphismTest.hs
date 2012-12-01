{-# LANGUAGE UnboxedTuples #-}
module Math.KnotTh.Tangles.IsomorphismTest
	( isomorphismTest
	, isomorphismTest'
	) where

import Data.Bits (shiftL)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray, STUArray, newArray, newArray_)
import Control.Monad.ST (ST, runST)
import Control.Monad (when)
import Math.Algebra.RotationDirection (ccw, cw)
import Math.KnotTh.Tangles


isomorphismTest' :: (CrossingType ct) => Tangle ct -> UArray Int Int
isomorphismTest' tangle = isomorphismTest (tangle, 0)


isomorphismTest :: (CrossingType ct) => (Tangle ct, Int) -> UArray Int Int
isomorphismTest (tangle, circles) = minimum [ min (code leg ccw) (code leg cw) | leg <- allLegs tangle ]
	where
		n = numberOfCrossings tangle

		code leg !dir = runST $ do
			x <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
			unsafeWrite x (crossingIndex $! adjacentCrossing leg) 1

			q <- newArray_ (0, n - 1) :: ST s (STArray s Int (Dart ct))
			unsafeWrite q 0 (opposite leg)
			free <- newSTRef 2

			let	{-# INLINE look #-}
				look !d !s
					| isLeg d    = return $! s `shiftL` 7
					| otherwise  = do
						let u = incidentCrossing d
						ux <- unsafeRead x (crossingIndex u)
						if ux > 0
							then return $! ux + (s `shiftL` 7)
							else do
								nf <- readSTRef free
								writeSTRef free $! nf + 1
								unsafeWrite x (crossingIndex u) nf
								unsafeWrite q (nf - 1) d
								return $! nf + (s `shiftL` 7)

			rc <- newArray (0, 2 * n - 1) 0 :: ST s (STUArray s Int Int)

			let	{-# INLINE bfs #-}
				bfs !h = when (h < n) $ do
					d <- unsafeRead q h
					nb <- foldMAdjacentDartsFrom d dir look 0
					case crossingCode dir d of
						(# be, le #) -> do
							unsafeWrite rc (2 * h) be
							unsafeWrite rc (2 * h + 1) $! le + nb `shiftL` 3
					bfs $! h + 1

			bfs 0
			unsafeFreeze rc
