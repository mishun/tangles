{-# LANGUAGE TemplateHaskell #-}
module Math.KnotTh.Link
	( module Math.KnotTh.Knotted
	, Link
	, Crossing
	, Dart
	, crossingLink
	, dartLink
	, fromList
	, fromListST
	, toList
	, allThreads
	) where

import Data.List (foldl')
import Data.Bits ((.&.), shiftL, shiftR, complement)
import qualified Data.Set as Set
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray, STUArray, runSTArray, newArray_)
import Data.Array.Base (unsafeAt, unsafeWrite, unsafeRead)
import Data.Array.Unsafe (unsafeFreeze)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, forM_)
import Math.Algebra.RotationDirection
import Math.KnotTh.Knotted
import Math.KnotTh.Knotted.TH.Link


data Dart ct = Dart !(Link ct) {-# UNPACK #-} !Int


instance Eq (Dart ct) where
	(==) (Dart _ d1) (Dart _ d2) = (d1 == d2)


instance Ord (Dart ct) where
	compare (Dart _ d1) (Dart _ d2) = compare d1 d2


data Crossing ct = Crossing !(Link ct) {-# UNPACK #-} !Int


instance Eq (Crossing ct) where
	(==) (Crossing _ c1) (Crossing _ c2) = (c1 == c2)


instance Ord (Crossing ct) where
	compare (Crossing _ c1) (Crossing _ c2) = compare c1 c2


data Link ct = Link
	{ crossCount     :: {-# UNPACK #-} !Int
	, crossingsArray :: {-# UNPACK #-} !(UArray Int Int)
	, stateArray     :: {-# UNPACK #-} !(Array Int (CrossingState ct))
	, loopsCount     :: {-# UNPACK #-} !Int
	}


instance Knotted Link Crossing Dart where
	numberOfFreeLoops = loopsCount

	numberOfCrossings = crossCount

	numberOfEdges link = 2 * (numberOfCrossings link)

	nthCrossing link i
		| i < 1 || i > numberOfCrossings link  = error "nthCrossing: out of bound"
		| otherwise                            = Crossing link (i - 1)

	mapCrossingStates f link = link
		{ stateArray = runSTArray $ do
			let n = numberOfCrossings link
			st <- newArray_ (0, n - 1)
			forM_ [0 .. n - 1] $ \ !i ->
				unsafeWrite st i $! f $! stateArray link `unsafeAt` i
			return $! st
		}

	crossingOwner = crossingLink

	crossingIndex (Crossing _ c) = c + 1

	crossingState (Crossing l c) = stateArray l `unsafeAt` c

	nthIncidentDart (Crossing l c) i = Dart l $! (c `shiftL` 2) + (i .&. 3)

	opposite (Dart l d) = Dart l $! crossingsArray l `unsafeAt` d

	nextCCW (Dart l d) = Dart l $! (d .&. complement 3) + ((d + 1) .&. 3)

	nextCW (Dart l d) = Dart l $! (d .&. complement 3) + ((d - 1) .&. 3)

	incidentCrossing (Dart l d) = Crossing l $! d `shiftR` 2

	dartPlace (Dart _ d) = d .&. 3

	dartOwner = dartLink

	dartArrIndex (Dart _ i) = i


instance KnottedWithAccel Link Crossing Dart where
	forMIncidentDarts (Crossing l c) f =
		let b = c `shiftL` 2
		in f (Dart l b) >> f (Dart l $! b + 1) >> f (Dart l $! b + 2) >> f (Dart l $! b + 3)

	foldMIncidentDarts (Crossing l c) f s =
		let b = c `shiftL` 2
		in f (Dart l b) s >>= f (Dart l $! b + 1) >>= f (Dart l $! b + 2) >>= f (Dart l $! b + 3)

	foldMIncidentDartsFrom dart@(Dart l i) !direction f s =
		let	d = directionSign direction
			r = i .&. complement 3
		in f dart s >>= f (Dart l $! r + ((i + d) .&. 3)) >>= f (Dart l $! r + ((i + 2 * d) .&. 3)) >>= f (Dart l $! r + ((i + 3 * d) .&. 3))


instance KnottedWithToPair Link Crossing Dart


{-# INLINE crossingLink #-}
crossingLink :: Crossing ct -> Link ct
crossingLink (Crossing l _) = l


{-# INLINE dartLink #-}
dartLink :: Dart ct -> Link ct
dartLink (Dart l _) = l


fromList :: (Int, [([(Int, Int)], CrossingState ct)]) -> Link ct
fromList list = runST $ fromListST list


fromListST :: (Int, [([(Int, Int)], CrossingState ct)]) -> ST s (Link ct)
fromListST (!loops, !list) = do
	when (loops < 0) (fail "fromListST: number of free loops is negative")

	let n = length list
	cr <- newArray_ (0, 4 * n - 1) :: ST s (STUArray s Int Int)
	st <- newArray_ (0, n - 1) :: ST s (STArray s Int a)

	forM_ (zip list [0 ..]) $ \ ((!ns, !state), !i) -> do
		unsafeWrite st i state
		when (length ns /= 4) (fail "fromList: there must be 4 neighbours for every crossing")
		forM_ (zip ns [0 ..]) $ \ ((!c, !p), !j) -> do
			when (c < 1 || c > n || p < 0 || p > 3) (fail "fromList: out of bound")
			let a = 4 * i + j
			let b = 4 * (c - 1) + p
			when (a == b) (fail "fromList: dart connected to itself")
			unsafeWrite cr a b
			when (b < a) $ do
				x <- unsafeRead cr b
				when (x /= a) (fail "fromList: unconsistent data")

	cr' <- unsafeFreeze cr
	st' <- unsafeFreeze st
	return $! Link
		{ crossCount     = n
		, crossingsArray = cr'
		, stateArray     = st'
		, loopsCount     = loops
		}


toList :: (CrossingType ct) => Link ct -> [([(Int, Int)], CrossingState ct)]
toList = map (\ c -> (map (toPair . opposite) $ incidentDarts c, crossingState c)) . allCrossings


allThreads :: Link ct -> [[(Dart ct, Dart ct)]]
allThreads =
	let extractThread (threads, vis) start
		| Set.member start vis  = (threads, vis)
		| otherwise             =
			let thread =
				let walk list d
					| d == start  = list
					| otherwise   = walk ((opposite d, d) : list) $ continuation $ opposite d
				in walk [(opposite start, start)] $ continuation $ opposite start
			in (thread : threads, foldl' (\ s (a, b) -> Set.insert b $ Set.insert a s) vis thread)
	in fst . foldl extractThread ([], Set.empty) . allDarts


$(mapM (\ (f, x) -> f x) [(produceShowDart, ''Dart), (produceShowCrossing, ''Crossing), (produceShowKnot, ''Link)])
