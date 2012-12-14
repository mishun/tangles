{-# LANGUAGE UnboxedTuples #-}
module Math.KnotTh.Tangle.BorderIncremental.RootingTest
	( rootingTest
	, rootCodeLeg
	, minimumRootCode
	) where

import Data.Bits (shiftL)
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray, STUArray, runSTUArray, newArray, newArray_)
import Data.Array.Unsafe (unsafeFreeze)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, foldM)
import Control.Applicative ((<$>))
import Math.Algebra.RotationDirection
import Math.Algebra.Group.Dn (DnSubGroup, fromPeriod, fromPeriodAndMirroredZero)
import Math.KnotTh.Tangle


rootingTest :: (CrossingType ct) => Crossing ct -> Maybe DnSubGroup
rootingTest lastCrossing = do
	let tangle = crossingTangle lastCrossing
	when (numberOfLegs tangle < 4) Nothing
	cp <- investigateConnectivity lastCrossing
	analyseSymmetry lastCrossing (unsafeAt cp . crossingIndex)


investigateConnectivity :: Crossing ct -> Maybe (UArray Int Bool)
investigateConnectivity lastCrossing = runST $ do
	let tangle = crossingTangle lastCrossing
	let n = numberOfCrossings tangle

	tins <- newArray (0, n) (-1) :: ST s (STUArray s Int Int)
	cp <- newArray (0, n) False :: ST s (STUArray s Int Bool)
	timer <- newSTRef 1

	let	{-# INLINE dfs #-}
		dfs !v !from = do
			tin <- readSTRef timer
			writeSTRef timer $! tin + 1
			unsafeWrite tins (crossingIndex v) tin

			let	{-# INLINE walk #-}
				walk !d (!fup, !border)
					| isLeg d    = return $! (fup, border + 1)
					| u == from  = return $! (fup, border)
					| otherwise  = do
						utin <- unsafeRead tins (crossingIndex u)
						if utin > 0
							then return $! (min fup utin, border)
							else do
								(!thatFup, !thatBorder) <- dfs u v
								when (thatFup >= tin) (unsafeWrite cp (crossingIndex v) True)
								return $! (min fup thatFup, border + if thatFup <= tin then thatBorder else 1)
					where
						u = incidentCrossing d

			foldMAdjacentDarts v walk (tin, 0 :: Int)

	(!_, !borderCut) <- dfs lastCrossing lastCrossing
	if borderCut <= 2
		then return $! Nothing
		else do
			unsafeWrite cp (crossingIndex lastCrossing) False
			(Just $!) <$> unsafeFreeze cp


analyseSymmetry :: (CrossingType ct) => Crossing ct -> (Crossing ct -> Bool) -> Maybe DnSubGroup
analyseSymmetry lastCrossing skipCrossing = findSymmetry
	where
		tangle = crossingTangle lastCrossing

		lastRootCode = min rcCCW rcCW
			where
				startCCW = firstLeg tangle
				startCW = opposite $ nthIncidentDart lastCrossing 3
				rcCCW = rootCodeLeg startCCW ccw
				rcCW = rootCodeLeg startCW cw

		findSymmetry =
			finalState >>= \ (symmetryDir, symmetryRev, positionDir, positionRev) ->
				let	l = numberOfLegs tangle
					period = div l (max symmetryDir symmetryRev)
				in if symmetryDir == symmetryRev
					then Just $! fromPeriodAndMirroredZero l period (positionRev + positionDir)
					else Just $! fromPeriod l period
			where
				finalState = foldM analyseLeg (0, 0, 0, 0) $! allLegs tangle

				testCW leg st@(symDir, symRev, posDir, _) =
					case compare (rootCodeLeg leg cw) lastRootCode of
						LT -> Nothing
						GT -> Just st
						EQ -> Just (symDir, symRev + 1, posDir, legPlace leg)

				testCCW leg st@(symDir, symRev, _, posRev) =
					case compare (rootCodeLeg leg ccw) lastRootCode of
						LT -> Nothing
						GT -> Just st
						EQ -> Just (symDir + 1, symRev, legPlace leg, posRev)

				analyseLeg state0 leg =
					if skipCrossing cur
						then Just state0
						else do
							state1 <- if nx /= cur then testCW leg state0 else return state0
							state2 <- if pv /= cur then testCCW leg state1 else return state1
							Just state2
					where
						cur = fst $ begin $ opposite leg
						nx = fst $ begin $ opposite $ nextCCW leg
						pv = fst $ begin $ opposite $ nextCW leg


rootCodeLeg :: (CrossingType ct) => Dart ct -> RotationDirection -> UArray Int Int
rootCodeLeg !root !dir = runSTUArray $ do
	when (isDart root) (fail "rootCodeLeg: leg expected")

	let tangle = dartTangle root
	let n = numberOfCrossings tangle

	when (numberOfFreeLoops tangle /= 0) (fail "rootCodeLeg: free loops present")
	when (numberOfCrossings tangle > 127) (fail "rootCodeLeg: too many crossings")

	x <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
	unsafeWrite x (crossingIndex $! adjacentCrossing root) 1
	q <- newArray_ (0, n - 1) :: ST s (STArray s Int (Dart ct))
	unsafeWrite q 0 (opposite root)
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
	return $! rc


minimumRootCode :: (CrossingType ct) => Tangle ct -> UArray Int Int
minimumRootCode tangle = minimum [ rootCodeLeg leg dir | leg <- allLegs tangle, dir <- bothDirections ]
