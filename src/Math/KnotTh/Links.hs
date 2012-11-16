module Math.KnotTh.Links
	( Link
	, Crossing
	, Dart

	, numberOfCrossings
	, crossingLink
	, crossingIndex
	, crossingState
	, dartLink
	, incidentCrossing
	, dartPlace
	, opposite
	, nextCCW
	, nextCW
	, nthIncidentDart
	, nthCrossing
	, foldMIncidentDarts
	, foldMAdjacentDarts
	, foldMIncidentDartsFrom
	, foldMAdjacentDartsFrom
	, mapCrossingStates
	, fromList

	, numberOfEdges
	, continuation
	, begin
	, adjacentCrossing
	, incidentDarts
	, nthAdjacentDart
	, adjacentDarts
	, allCrossings
	, allDarts
	) where

import Data.List (intercalate)
import Data.Bits ((.&.), shiftL, shiftR, complement)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray, STUArray, newArray_)
import Data.Array.Base (unsafeAt, unsafeWrite, unsafeRead)
import Data.Array.Unsafe (unsafeFreeze)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, forM_)
import Math.Algebra.RotationDirection
import Math.KnotTh.Crossings


data Dart ct = Dart !(Link ct) {-# UNPACK #-} !Int


instance Eq (Dart ct) where
	(==) (Dart _ d1) (Dart _ d2) = (d1 == d2)


instance Ord (Dart ct) where
	compare (Dart _ d1) (Dart _ d2) = compare d1 d2


instance Show (Dart ct) where
	show d =
		let (c, p) = begin d
		in concat ["(Dart ", show (crossingIndex c), " ", show p, ")"]


data Crossing ct = Crossing !(Link ct) {-# UNPACK #-} !Int


instance Eq (Crossing ct) where
	(==) (Crossing _ c1) (Crossing _ c2) = (c1 == c2)


instance Ord (Crossing ct) where
	compare (Crossing _ c1) (Crossing _ c2) = compare c1 c2


instance Show (Crossing ct) where
	show c = concat ["(Crossing ", show (crossingIndex c), " [ ", intercalate " " $ map (show . opposite . nthIncidentDart c) [0 .. 3], " ])"]


data Link ct = Link
	{ numberOfCrossings :: {-# UNPACK #-} !Int
	, crossingsArray    :: {-# UNPACK #-} !(UArray Int Int)
	, stateArray        :: {-# UNPACK #-} !(Array Int (CrossingState ct))
	}


instance Show (Link ct) where
	show t =
		let d = map (show . nthCrossing t) [1 .. numberOfCrossings t]
		in concat ["(Tangle ", intercalate " " d, " )"]


{-# INLINE crossingLink #-}
crossingLink :: Crossing ct -> Link ct
crossingLink (Crossing l _) = l


{-# INLINE crossingIndex #-}
crossingIndex :: Crossing ct -> Int
crossingIndex (Crossing _ c) = c


{-# INLINE crossingState #-}
crossingState :: Crossing ct -> CrossingState ct
crossingState (Crossing l c) = stateArray l `unsafeAt` (c - 1)


{-# INLINE dartLink #-}
dartLink :: Dart ct -> Link ct
dartLink (Dart l _) = l


{-# INLINE incidentCrossing #-}
incidentCrossing :: Dart ct -> Crossing ct
incidentCrossing (Dart l d) = Crossing l $! 1 + d `shiftR` 2


{-# INLINE dartPlace #-}
dartPlace :: Dart ct -> Int
dartPlace (Dart _ d) = d .&. 3


{-# INLINE opposite #-}
opposite :: Dart ct -> Dart ct
opposite (Dart l d) = Dart l $! crossingsArray l `unsafeAt` d


{-# INLINE nextCCW #-}
nextCCW :: Dart ct -> Dart ct
nextCCW (Dart l d) = Dart l $! (d .&. complement 3) + ((d + 1) .&. 3)


{-# INLINE nextCW #-}
nextCW :: Dart ct -> Dart ct
nextCW (Dart l d) = Dart l $! (d .&. complement 3) + ((d - 1) .&. 3)


{-# INLINE nthIncidentDart #-}
nthIncidentDart :: Crossing ct -> Int -> Dart ct
nthIncidentDart (Crossing l c) i = Dart l $! ((c - 1) `shiftL` 2) + (i .&. 3)


{-# INLINE nthCrossing #-}
nthCrossing :: Link ct -> Int -> Crossing ct
nthCrossing l i
	| i < 1 || i > numberOfCrossings l  = error "nthCrossing: out of bound"
	| otherwise                         = Crossing l i


{-# INLINE foldMIncidentDarts #-}
foldMIncidentDarts :: (Monad m) => Crossing ct -> (Dart ct -> s -> m s) -> s -> m s
foldMIncidentDarts (Crossing l c) f s =
	let b = (c - 1) `shiftL` 2
	in f (Dart l b) s >>= f (Dart l $! b + 1) >>= f (Dart l $! b + 2) >>= f (Dart l $! b + 3)


{-# INLINE foldMAdjacentDarts #-}
foldMAdjacentDarts :: (Monad m) => Crossing ct -> (Dart ct -> s -> m s) -> s -> m s
foldMAdjacentDarts (Crossing l c) f s =
	let b = (c - 1) `shiftL` 2
	in f (opposite $! Dart l b) s >>= f (opposite $! Dart l $! b + 1) >>= f (opposite $! Dart l $! b + 2) >>= f (opposite $! Dart l $! b + 3)


{-# INLINE foldMIncidentDartsFrom #-}
foldMIncidentDartsFrom :: (Monad m) => Dart ct -> RotationDirection -> (Dart ct -> s -> m s) -> s -> m s
foldMIncidentDartsFrom dart@(Dart l i) !direction f s =
	let	d = directionSign direction
		r = i .&. complement 3
	in f dart s >>= f (Dart l $! r + ((i + d) .&. 3)) >>= f (Dart l $! r + ((i + 2 * d) .&. 3)) >>= f (Dart l $! r + ((i + 3 * d) .&. 3))


{-# INLINE foldMAdjacentDartsFrom #-}
foldMAdjacentDartsFrom :: (Monad m) => Dart ct -> RotationDirection -> (Dart ct -> s -> m s) -> s -> m s
foldMAdjacentDartsFrom dart@(Dart l i) !direction f s =
	let	d = directionSign direction
		r = i .&. complement 3
	in f (opposite dart) s >>= f (opposite $! Dart l $! r + ((i + d) .&. 3))
		>>= f (opposite $! Dart l $! r + ((i + 2 * d) .&. 3))
			>>= f (opposite $! Dart l $! r + ((i + 3 * d) .&. 3))


mapCrossingStates :: (CrossingState a -> CrossingState b) -> Link a -> Link b
mapCrossingStates f link = runST $ do
	let n = numberOfCrossings link
	st <- newArray_ (0, n - 1) :: ST s (STArray s Int a)
	forM_ [0 .. n - 1] $ \ !i ->
		unsafeWrite st i $! f $! stateArray link `unsafeAt` i
	st' <- unsafeFreeze st
	return $! Link
		{ numberOfCrossings = n
		, crossingsArray    = crossingsArray link
		, stateArray        = st' 
		}


fromList :: [([(Int, Int)], CrossingState ct)] -> Link ct
fromList list = runST $ do
	let n = length list
	cr <- newArray_ (0, 8 * n - 1) :: ST s (STUArray s Int Int)
	st <- newArray_ (0, n - 1) :: ST s (STArray s Int a)
	forM_ (zip list [0 ..]) $ \ ((!ns, !state), !i) -> do
		unsafeWrite st i state
		when (length ns /= 4) (fail "fromList: there must be 4 neighbours for every crossing")
		forM_ (zip ns [0 ..]) $ \ ((!c, !p), !j) -> do
			when (c < 1 || c > n || p < 0 || p > 3) (fail "fromList: out of bound")
			when (c == i + 1 && p == j) (fail "fromList: dart connected to itself")
			when (c <= i || (c == i + 1 && p < j)) $ do
				let offset = 4 * (c - 1) + p
				c' <- unsafeRead cr (2 * offset)
				p' <- unsafeRead cr (2 * offset + 1)
				when (c' /= i + 1 || p' /= j) (fail "fromList: unconsistent data")
			let offset = 4 * i + j
			unsafeWrite cr (2 * offset) c
			unsafeWrite cr (2 * offset + 1) p

	cr' <- unsafeFreeze cr
	st' <- unsafeFreeze st
	return $! Link
		{ numberOfCrossings = n
		, crossingsArray    = cr'
		, stateArray        = st'
		}



{-# INLINE numberOfEdges #-}
numberOfEdges :: Link ct -> Int
numberOfEdges l = 2 * (numberOfCrossings l)


{-# INLINE continuation #-}
continuation :: Dart ct -> Dart ct
continuation d = nextCCW $! nextCCW d


{-# INLINE begin #-}
begin :: Dart ct -> (Crossing ct, Int)
begin d =
	let	c = incidentCrossing d
		p = dartPlace d
	in c `seq` p `seq` (c, p)


{-# INLINE adjacentCrossing #-}
adjacentCrossing :: Dart ct -> Crossing ct
adjacentCrossing = incidentCrossing . opposite


{-# INLINE incidentDarts #-}
incidentDarts :: Crossing ct -> [Dart ct]
incidentDarts c = map (nthIncidentDart c) [0 .. 3]


{-# INLINE nthAdjacentDart #-}
nthAdjacentDart :: Crossing ct -> Int -> Dart ct
nthAdjacentDart c = opposite . nthIncidentDart c


{-# INLINE adjacentDarts #-}
adjacentDarts :: Crossing ct -> [Dart ct]
adjacentDarts c = map (nthAdjacentDart c) [0 .. 3]


{-# INLINE allCrossings #-}
allCrossings :: Link ct -> [Crossing ct]
allCrossings t = map (nthCrossing t) [1 .. numberOfCrossings t]


{-# INLINE allDarts #-}
allDarts :: Link ct -> [Dart ct]
allDarts = concatMap incidentDarts . allCrossings
