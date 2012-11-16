module Math.KnotTh.Tangles.BorderIncremental.IncrementalTests
	( testInTriangle
	, testMultiEdges
	, testFlow4
	) where

import Data.Array.Base (newArray, newArray_, unsafeRead, unsafeWrite)
import Data.Array.ST (STArray, STUArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, forM_)
import Math.KnotTh.Tangles


testInTriangle :: Int -> Dart ct -> Int -> Bool
testInTriangle maxN leg gl =
	let	t = dartTangle leg
		n = numberOfCrossings t
		l = numberOfLegs t
	in (l >= 2 * gl) && (n + div l 2 - gl < maxN)


testMultiEdges :: Dart ct -> Int -> Bool
testMultiEdges leg gl =
	let ls = take gl $! iterate nextCW leg
	in and $! zipWith (\ !a !b ->
			let	a' = opposite a
				b' = opposite b
			in isLeg a' || isLeg b' || incidentCrossing a' /= incidentCrossing b' 
		) ls $! tail ls


testFlow4 :: Crossing ct -> Bool
testFlow4 finish = runST $ do
	let tangle = crossingTangle finish
	let n = numberOfCrossings tangle
	let l = numberOfLegs tangle

	flow <- newArray (0, 4 * n - 1) 0 :: ST s (STUArray s Int Int)
	total <- newSTRef =<<
		foldMIncidentDarts finish (\ !d !f ->
			if isLeg (opposite d)
				then unsafeWrite flow (dartArrIndex d) (-1) >> (return $! f + 1)
				else return $! f
		) (0 :: Int)

	let push = do
		v <- newArray (0, n) False :: ST s (STUArray s Int Bool)
		p <- newArray_ (0, n)  :: ST s (STArray s Int (Dart ct))
		q <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
		tl <- newSTRef 0

		let touch !d = do
			let ci = crossingIndex $! incidentCrossing d
			visited <- unsafeRead v ci
			when (not visited) $ do
				unsafeWrite v ci True
				unsafeWrite p ci d
				i <- readSTRef tl
				unsafeWrite q i ci
				writeSTRef tl $! i + 1

		forM_ (allLegs tangle) $ \ !a -> do
			let b = opposite a
			when (isDart b) $ do
				f <- unsafeRead flow $! dartArrIndex b
				when (f > -1) (touch b)

		when (l == 4) $ do
			let d = opposite $! nthLeg tangle 2
			when (isDart d) (touch d)

		let loop !h = do
			cont <- readSTRef tl >>= \ !t -> return $! (t > h)
			when cont $ do
				ci <- unsafeRead q h
				forMIncidentDarts (nthCrossing tangle ci) $ \ !a -> do
					let b = opposite a
					when (isDart b) $ do
						f <- unsafeRead flow $! dartArrIndex b
						when (f > -1) (touch b)
				loop $! h + 1

		loop 0

		pathFound <- unsafeRead v $! crossingIndex finish
		if pathFound
			then do
				let update !a = do
					unsafeRead flow (dartArrIndex a) >>= \ !f -> unsafeWrite flow (dartArrIndex a) $! f - 1
					let b = opposite a
					when (isDart b) $ do
						unsafeRead flow (dartArrIndex b) >>= \ !f -> unsafeWrite flow (dartArrIndex b) $! f + 1
						unsafeRead p (crossingIndex $! incidentCrossing b) >>= update

				unsafeRead p (crossingIndex finish) >>= update
				readSTRef total >>= \ !f -> writeSTRef total $! f + 1
				push
			else do
				final <- readSTRef total
				foldMIncidentDarts finish (\ !a !ok -> do
						let b = opposite a
						if isDart b
							then unsafeRead v (crossingIndex $! incidentCrossing b) >>= \ !ok' -> return $! ok' && ok
							else return $! ok
					) (final == 4)

	push
