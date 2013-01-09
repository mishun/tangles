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
import Control.Monad (forM_, when, foldM_, foldM, filterM, void)
import Text.Printf
import Math.Algebra.RotationDirection (RotationDirection, ccw, cw)
import Math.KnotTh.Tangle


isomorphismTest :: (CrossingType ct) => Tangle ct -> UArray Int Int
isomorphismTest tangle
	| numberOfCrossings tangle > 127  = error $ printf "isomorphismTest: too many crossings (%i)" (numberOfCrossings tangle)
	| otherwise                       = min (codeWithDirection ccw tangle) (codeWithDirection cw tangle)


codeWithDirection :: (CrossingType ct) => RotationDirection -> Tangle ct -> UArray Int Int
codeWithDirection !dir tangle = minimum $ map code $ allLegs tangle
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

			let bfs !st !head = do
				tail <- readSTRef free
				if head >= tail - 1 then return $! st else do
					input <- unsafeRead queue head
					!nb <- foldMAdjacentDartsFrom input dir lookAndAdd 0
					case crossingCode dir input of
						(# be, le'' #) -> do
							let le = le'' + shiftL nb 3
							    bi = l + 1 + 2 * head
							    li = bi + 1
							case st of
								LT -> unsafeWrite rc bi be >> unsafeWrite rc li le >> bfs LT (head + 1)
								EQ -> do
									be' <- unsafeRead rc bi
									case compare be be' of
										LT -> unsafeWrite rc bi be >> unsafeWrite rc li le >> bfs LT (head + 1)
										EQ -> do
											le' <- unsafeRead rc li
											case compare le le' of
												LT -> unsafeWrite rc li le >> bfs LT (head + 1)
												EQ -> bfs EQ (head + 1)
												GT -> return GT
										GT -> return GT
								GT -> return GT

			LT <- bfs LT 0
			fix $ \ recheck -> do
				tail <- readSTRef free
				when (tail <= n) $ do
					notVisited <- filterM (\ !i -> unsafeRead index i >>= (return . (== 0))) [1 .. n]

					(d, _) <- foldM (\ (pd, !st) !d -> do
							writeSTRef free tail
							forM_ notVisited $ \ !i -> unsafeWrite index i 0
							void $ look d
							r <- bfs st (tail - 1)
							case r of
								LT -> return $! (d, EQ)
								_  -> return $! (pd, EQ)
						)
						(undefined, LT)
						[d | i <- notVisited, d <- incidentDarts (nthCrossing tangle i)]

					writeSTRef free tail
					forM_ notVisited $ \ !i -> unsafeWrite index i 0
					void $ look d
					LT <- bfs LT (tail - 1)

					--fail "codeWithDirection: disconnected diagram (not implemented)"
					recheck

			return $! rc
