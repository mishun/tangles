{-# LANGUAGE UnboxedTuples #-}
module Math.KnotTh.Tangle.IsomorphismTest
	( isomorphismTest
	) where

import Prelude hiding (head, tail)
import Data.Bits (shiftL)
import Data.Function (fix)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Array.Base (unsafeRead, unsafeWrite)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray, STUArray, runSTUArray, newArray, newArray_)
import Control.Monad.ST (ST)
import Control.Monad (when, foldM_)
import Math.Algebra.RotationDirection (RotationDirection, ccw, cw)
import Math.KnotTh.Tangle


isomorphismTest :: (CrossingType ct) => Tangle ct -> UArray Int Int
isomorphismTest tangle
	| numberOfCrossings tangle > 127  = error "isomorphismTest: too many crossings"
	| otherwise                       = min (codeWithDirection ccw tangle) (codeWithDirection cw tangle)


codeWithDirection :: (CrossingType ct) => RotationDirection -> Tangle ct -> UArray Int Int
codeWithDirection !dir tangle = minimum [ code leg | leg <- allLegs tangle ]
	where
		n = numberOfCrossings tangle
		l = numberOfLegs tangle

		code leg = runSTUArray $ do
			index <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
			queue <- newArray_ (0, n - 1) :: ST s (STArray s Int (Dart ct))
			free <- newSTRef 1

			let	{-# INLINE look #-}
				look !d
					| isLeg d    = return 0
					| otherwise  = do
						let u = incidentCrossing d
						ux <- unsafeRead index $! crossingIndex u
						if ux > 0
							then return $! ux
							else do
								nf <- readSTRef free
								writeSTRef free $! nf + 1
								unsafeWrite index (crossingIndex u) nf
								unsafeWrite queue (nf - 1) d
								return $! nf

				{-# INLINE lookAndAdd #-}
				lookAndAdd !d !s = do
					!c <- look d
					return $! c + s `shiftL` 7

			rc <- newArray_ (0, l + 2 * n) :: ST s (STUArray s Int Int)
			unsafeWrite rc 0 $! numberOfFreeLoops tangle
			foldM_ (\ !d !i -> do { look (opposite d) >>= unsafeWrite rc i ; return $! nextDir dir d }) leg [1 .. l]

			flip fix 0 $ \ bfs !head -> do
				tail <- readSTRef free
				when (head < tail - 1) $ do
					input <- unsafeRead queue head
					!nb <- foldMAdjacentDartsFrom input dir lookAndAdd 0
					case crossingCode dir input of
						(# be, le #) -> do
							unsafeWrite rc (l + 1 + 2 * head) be
							unsafeWrite rc (l + 2 + 2 * head) $! le + nb `shiftL` 3
					bfs $! head + 1

			fix $ \ recheck -> do
				tail <- readSTRef free
				when (tail <= n) $
					fail "codeWithDirection: disconnected diagram (not implemented)"
					recheck

			return $! rc
