module Math.KnotTh.Knotted.Threads
	( ThreadedCrossing(..)
	, maybeContinuation
	, allThreads
	, allThreadsWithMarks
	) where

import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Array.Unboxed (UArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Control.Monad.ST
import Control.Monad (foldM_)
import Math.KnotTh.Knotted.Knotted
import Math.KnotTh.Knotted.Util


class (CrossingType ct) => ThreadedCrossing ct where
	continuation :: (Knotted k c d) => d ct -> d ct

	continuation d
		| isDart d   = nextCCW $ nextCCW d
		| otherwise  = error "continuation: from endpoint"


{-# INLINE maybeContinuation #-}
maybeContinuation :: (ThreadedCrossing ct, Knotted k c d) => d ct -> Maybe (d ct)
maybeContinuation d
	| isDart d   = Just $! continuation d
	| otherwise  = Nothing


allThreads :: (ThreadedCrossing ct, Knotted k c d, Eq (d ct)) => k ct -> [[(d ct, d ct)]]
allThreads = snd . allThreadsWithMarks


allThreadsWithMarks :: (ThreadedCrossing ct, Knotted k c d, Eq (d ct)) => k ct -> (UArray Int Int, [[(d ct, d ct)]])
allThreadsWithMarks knot = runST $ do
	visited <- newArray (dartIndexRange knot) 0 :: ST s (STUArray s Int Int)
	threads <- newSTRef $ replicate (numberOfFreeLoops knot) []

	flip (flip foldM_ 1) (allEdges knot) $ \ !i (!startA, !startB) -> do
		v <- readArray visited $ dartIndex startA
		if v /= 0
			then return $! i
			else do
				let traceBack !prev !b = do
					let a = opposite b
					writeArray visited (dartIndex a) i
					writeArray visited (dartIndex b) (-i)
					let !next = (a, b) : prev
					if isEndpoint a
						then return $! Right $! next
						else do
							let b' = continuation a
							if b' == startB
								then return $! Left $! next
								else traceBack next b'

				let traceFront !prev !b'
					| isEndpoint b'  = return $! reverse prev
					| otherwise      = do
						let !a = continuation b'
						let !b = opposite a
						writeArray visited (dartIndex a) i
						writeArray visited (dartIndex b) (-i)
						traceFront ((a, b) : prev) b

				tb <- traceBack [] startB
				case tb of
					Left thread  -> readSTRef threads >>= \ !list -> writeSTRef threads $! thread : list
					Right prefix -> do
						!suffix <- traceFront [] startB
						readSTRef threads >>= \ !list ->
							writeSTRef threads $! (prefix ++ suffix) : list

				return $! i + 1

	visited' <- unsafeFreeze visited
	(,) visited' `fmap` readSTRef threads
