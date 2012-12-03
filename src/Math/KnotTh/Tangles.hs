module Math.KnotTh.Tangles
	( module Math.KnotTh.Knotted
	, Dart
	, Crossing
	, Tangle
	, crossingTangle
	, dartTangle
	, numberOfLegs
	, isLeg
	, isDart
	, legPlace
	, nthLeg
	, firstLeg
	, allLegs
	, isAdjacentToBorder
	, maybeIncidentCrossing
	, maybeAdjacentCrossing
	, allLegsAndDarts
	, allEdges
	, lonerTangle
	, glueToBorder
	, glueToBorderST
	, fromLists
	, fromListsST
	, toPair
	, toLists
	) where

import Data.List (intercalate)
import Data.Array.IArray (listArray)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray, STUArray, runSTArray, newArray_)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.Base (unsafeAt, unsafeWrite, unsafeRead)
import Data.Bits ((.&.))
import Control.DeepSeq
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_, when)
import Math.Algebra.RotationDirection
import Math.KnotTh.Knotted


data Dart ct = Dart !(Tangle ct) {-# UNPACK #-} !Int {-# UNPACK #-} !Int


instance Eq (Dart ct) where
	(==) (Dart _ c1 p1) (Dart _ c2 p2) = (c1 == c2) && (p1 == p2)


instance Ord (Dart ct) where
	compare (Dart _ c1 p1) (Dart _ c2 p2) =
		case compare c1 c2 of
			EQ -> compare p1 p2
			r  -> r


instance Show (Dart ct) where
	show d
		| isLeg d    = concat ["(Leg ", show $ legPlace d, ")"]
		| otherwise  =
			let (c, p) = begin d
			in concat ["(Dart ", show $ crossingIndex c, " ", show p, ")"]


data Crossing ct = Crossing !(Tangle ct) {-# UNPACK #-} !Int


instance Eq (Crossing ct) where
	(==) (Crossing _ c1) (Crossing _ c2) = (c1 == c2)


instance Ord (Crossing ct) where
	compare (Crossing _ c1) (Crossing _ c2) = compare c1 c2


instance (Show ct, CrossingType ct) => Show (Crossing ct) where
	show c =
		let d = map (show . opposite) $ incidentDarts c
		in concat ["(Crossing ", show (crossingIndex c), " ",  show $ crossingState c," [ ", intercalate " " d, " ])"]


data Tangle ct = Tangle
	{ count          :: {-# UNPACK #-} !Int
	, numberOfLegs   :: {-# UNPACK #-} !Int
	, crossingsArray :: {-# UNPACK #-} !(UArray Int Int)
	, borderArray    :: {-# UNPACK #-} !(UArray Int Int)
	, stateArray     :: {-# UNPACK #-} !(Array Int (CrossingState ct))
	}


instance (NFData ct) => NFData (Tangle ct) where
	rnf tangle = rnf (stateArray tangle) `seq` tangle `seq` ()


instance (Show ct, CrossingType ct) => Show (Tangle ct) where
	show t =
		let d = concat ["(Border [ ", intercalate " " (map (show . opposite) $ allLegs t), " ])"] : map (show . nthCrossing t) [1 .. numberOfCrossings t]
		in concat ["(Tangle ", intercalate " " d, ")"]


instance Knotted Tangle Crossing Dart where
	numberOfCrossings = count

	numberOfEdges tangle = 2 * (numberOfCrossings tangle) + div (numberOfLegs tangle) 2

	nthCrossing t i
		| i < 1 || i > numberOfCrossings t  = error "nthCrossing: out of bound"
		| otherwise                         = Crossing t i

	mapCrossingStates f tangle = tangle
		{ stateArray = runSTArray $ do
			let n = numberOfCrossings tangle
			st <- newArray_ (0, n - 1)
			forM_ [0 .. n - 1] $ \ !i ->
				unsafeWrite st i $! f $! stateArray tangle `unsafeAt` i
			return $! st
		}

	crossingOwner = crossingTangle

	crossingIndex (Crossing _ c) = c

	crossingState (Crossing t c) = stateArray t `unsafeAt` (c - 1)

	nthIncidentDart (Crossing t c) i = Dart t c (i .&. 3)

	opposite (Dart t c i)
		| c == 0     =
			let	a = borderArray t
				j = 2 * i
			in Dart t (a `unsafeAt` j) (a `unsafeAt` (j + 1))
		| otherwise  =
			let	a = crossingsArray t
				j = (c - 1) * 8 + 2 * i
			in Dart t (a `unsafeAt` j) (a `unsafeAt` (j + 1))

	nextCCW d@(Dart t c i) =
		let u = if isLeg d then numberOfLegs t - 1 else 3
		in Dart t c (if i == u then 0 else i + 1)

	nextCW d@(Dart t c i) =
		let u = if isLeg d then numberOfLegs t - 1 else 3
		in Dart t c (if i == 0 then u else i - 1)

	incidentCrossing (Dart t c _)
		| c == 0     = error "incidentCrossing: from leg"
		| otherwise  = Crossing t c

	dartPlace (Dart _ c i)
		| c == 0     = error "dartPlace: from leg"
		| otherwise  = i

	dartOwner = dartTangle

	dartArrIndex (Dart t c i)
		| c == 0     = let n = numberOfCrossings t in 4 * n + i
		| otherwise  = 4 * (c - 1) + i

	forMIncidentDarts (Crossing t c) f = do{ f $! Dart t c 0 ; f $! Dart t c 1 ; f $! Dart t c 2 ; f $! Dart t c 3 }

	foldMIncidentDarts (Crossing t c) f s = f (Dart t c 0) s >>= f (Dart t c 1) >>= f (Dart t c 2) >>= f (Dart t c 3)

	foldMIncidentDartsFrom dart@(Dart t c i) !direction f s
		| isLeg dart  = error "foldMIncidentDartsFrom: from leg"
		| otherwise   =
			let d = directionSign direction
			in f dart s >>= f (Dart t c $! (i + d) .&. 3) >>= f (Dart t c $! (i + 2 * d) .&. 3) >>= f (Dart t c $! (i + 3 * d) .&. 3)


{-# INLINE crossingTangle #-}
crossingTangle :: Crossing ct -> Tangle ct
crossingTangle (Crossing t _) = t


{-# INLINE dartTangle #-}
dartTangle :: Dart ct -> Tangle ct
dartTangle (Dart t _ _) = t


{-# INLINE isLeg #-}
isLeg :: Dart ct -> Bool
isLeg (Dart _ c _) = (c == 0)


{-# INLINE isDart #-}
isDart :: Dart ct -> Bool
isDart (Dart _ c _) = (c /= 0)


{-# INLINE legPlace #-}
legPlace :: Dart ct -> Int
legPlace (Dart _ c i)
	| c /= 0     = error "borderPlace: from non-leg"
	| otherwise  = i


{-# INLINE nthLeg #-}
nthLeg :: Tangle ct -> Int -> Dart ct
nthLeg t i = let l = numberOfLegs t in Dart t 0 ((l + mod i l) `mod` l)


{-# INLINE firstLeg #-}
firstLeg :: Tangle ct -> Dart ct
firstLeg t = Dart t 0 0


{-# INLINE allLegs #-}
allLegs :: Tangle ct -> [Dart ct]
allLegs t = map (Dart t 0) [0 .. numberOfLegs t - 1]


{-# INLINE isAdjacentToBorder #-}
isAdjacentToBorder :: Dart ct -> Bool
isAdjacentToBorder = isLeg . opposite


{-# INLINE maybeIncidentCrossing #-}
maybeIncidentCrossing :: Dart ct -> Maybe (Crossing ct)
maybeIncidentCrossing d
	| isLeg d    = Nothing
	| otherwise  = Just $! incidentCrossing d


{-# INLINE maybeAdjacentCrossing #-}
maybeAdjacentCrossing :: Dart ct -> Maybe (Crossing ct)
maybeAdjacentCrossing = maybeIncidentCrossing . opposite


{-# INLINE allLegsAndDarts #-}
allLegsAndDarts :: Tangle ct -> [Dart ct]
allLegsAndDarts tangle = allLegs tangle ++ allDarts tangle


{-# INLINE allEdges #-}
allEdges :: Tangle ct -> [(Dart ct, Dart ct)]
allEdges tangle = [ (a, b) | a <- allLegsAndDarts tangle, let b = opposite a, a < b ]


lonerTangle :: (CrossingType ct) => CrossingState ct -> Tangle ct
lonerTangle !cr = Tangle
	{ count          = 1
	, numberOfLegs   = 4
	, crossingsArray = listArray (0, 7) [0, 0, 0, 1, 0, 2, 0, 3]
	, borderArray    = listArray (0, 7) [1, 0, 1, 1, 1, 2, 1, 3]
	, stateArray     = listArray (0, 0) $! [cr]
	}


--       edgesToGlue = 1                 edgesToGlue = 2                 edgesToGlue = 3
-- ........|                       ........|                       ........|
-- (leg+1)-|---------------3       (leg+1)-|---------------2       (leg+1)-|---------------1
--         |  +=========+                  |  +=========+                  |  +=========+
--  (leg)--|--|-0-\ /-3-|--2        (leg)--|--|-0-\ /-3-|--1        (leg)--|--|-0-\ /-3-|--0
-- ........|  |    *    |                  |  |    *    |                  |  |    *    |
-- ........|  |   / \-2-|--1       (leg-1)-|--|-1-/ \-2-|--0       (leg-1)-|--|-1-/ \   |
-- ........|  |  1      |          ........|  +=========+                  |  |      2  |
-- ........|  |   \-----|--0       ........|                       (leg-2)-|--|-----/   |
-- ........|  +=========+          ........|                       ........|  +=========+
glueToBorder :: (CrossingType ct) => Dart ct -> Int -> CrossingState ct -> Crossing ct
glueToBorder leg legsToGlue crossingToGlue = runST $ glueToBorderST leg legsToGlue crossingToGlue


glueToBorderST :: (CrossingType ct) => Dart ct -> Int -> CrossingState ct -> ST s (Crossing ct)
glueToBorderST leg legsToGlue crossingToGlue
	| not (isLeg leg)                   = error "glueToBorder: leg expected"
	| legsToGlue < 1 || legsToGlue > 3  = error "glueToBorder: legsToGlue must be 1, 2 or 3"
	| otherwise                         = do
		let tangle = dartTangle leg
		let oldL = numberOfLegs tangle
		when (oldL <= legsToGlue) (fail "glueToBorder: not enough legs to glue")

		let oldC = numberOfCrossings tangle
		let newC = oldC + 1
		let newL = oldL + 4 - 2 * legsToGlue
		let lp = legPlace leg

		cr <- newArray_ (0, 8 * newC - 1) :: ST s (STUArray s Int Int)
		ls <- newArray_ (0, 2 * newL - 1) :: ST s (STUArray s Int Int)


		let	{-# INLINE writePair #-}
			writePair :: STUArray s Int Int -> Int -> Int -> Int -> ST s ()
			writePair arr index c p = do
				unsafeWrite arr (2 * index) c
				unsafeWrite arr (2 * index + 1) p

		let	{-# INLINE copyModified #-}
			copyModified :: STUArray s Int Int -> Int -> UArray Int Int -> Int -> ST s ()
			copyModified !arr !index !arr' !index' = do
				let c = arr' `unsafeAt` (2 * index')
				let p = arr' `unsafeAt` (2 * index' + 1)
				if c == 0 then do
					let ml = (oldL + p - lp - 1) `mod` oldL
					if ml < oldL - legsToGlue
						then writePair arr index 0 (4 - legsToGlue + ml)
						else writePair arr index newC (oldL - 1 - ml)
				else writePair arr index c p

		forM_ [0 .. 4 * oldC - 1] $ \ !i ->
			copyModified cr i (crossingsArray tangle) i

		forM_ [0 .. legsToGlue - 1] $ \ !i ->
			copyModified cr (4 * (newC - 1) + i) (borderArray tangle) ((oldL + lp - i) `mod` oldL)

		forM_ [0 .. 3 - legsToGlue] $ \ !i -> do
			let j = i + legsToGlue
			writePair ls i newC j
			writePair cr (4 * (newC - 1) + j) 0 i

		forM_ [0 .. oldL - 1 - legsToGlue] $ \ !i ->
			copyModified ls (i + 4 - legsToGlue) (borderArray tangle) ((lp + 1 + i) `mod` oldL)

		st <- newArray_ (0, newC - 1) :: ST s (STArray s Int a)
		forM_ [0 .. oldC - 1] $ \ !i ->
			unsafeWrite st i $! stateArray tangle `unsafeAt` i
		unsafeWrite st (newC - 1) crossingToGlue

		cr' <- unsafeFreeze cr
		ls' <- unsafeFreeze ls
		st' <- unsafeFreeze st
		let result = Tangle
			{ count          = newC
			, numberOfLegs   = newL
			, crossingsArray = cr'
			, borderArray    = ls'
			, stateArray     = st' 
			}

		return $! nthCrossing result newC


fromLists :: [(Int, Int)] -> [([(Int, Int)], CrossingState ct)] -> Tangle ct
fromLists border list = runST $ fromListsST border list


fromListsST :: [(Int, Int)] -> [([(Int, Int)], CrossingState ct)] -> ST s (Tangle ct)
fromListsST border list = do
	let n = length list
	let l = length border
	when (l <= 0 || odd l) (fail "fromLists: number of legs must be positive and even")

	let	{-# INLINE testPair #-}
		testPair c p =
			case c of
				0                  -> when (p < 0 || p >= l) (fail "fromLists: leg index is out of bound")
				_ | c < 0 || c > n -> fail "fromLists: crossing index must be from 1 to number of crossings"
				  | otherwise      -> when (p < 0 || p > 3) (fail "fromLists: place index is out of bound")

	ls <- newArray_ (0, 2 * l - 1) :: ST s (STUArray s Int Int)
	forM_ (zip border [0 ..]) $ \ ((!c, !p), !i) -> do
		testPair c p
		when (c == 0 && p == i) (fail "fromLists: leg connected to itself")
		when (c == 0 && p < i) $ do
			c' <- unsafeRead ls (2 * p)
			p' <- unsafeRead ls (2 * p + 1)
			when (c' /= 0 || p' /= i) (fail "fromLists: unconsistent data")
		unsafeWrite ls (2 * i) c
		unsafeWrite ls (2 * i + 1) p

	cr <- newArray_ (0, 8 * n - 1) :: ST s (STUArray s Int Int)
	st <- newArray_ (0, n - 1) :: ST s (STArray s Int a)
	forM_ (zip list [0 ..]) $ \ ((!ns, !state), !i) -> do
		unsafeWrite st i state
		when (length ns /= 4) (fail "fromLists: there must be 4 neighbours for every crossing")
		forM_ (zip ns [0 ..]) $ \ ((!c, !p), !j) -> do
			testPair c p
			when (c == i + 1 && p == j) (fail "fromLists: dart connected to itself")
			when (c <= i || (c == i + 1 && p < j)) $ do
				let (a, offset) = if c == 0 then (ls, p) else (cr, 4 * (c - 1) + p)
				c' <- unsafeRead a (2 * offset)
				p' <- unsafeRead a (2 * offset + 1)
				when (c' /= i + 1 || p' /= j) (fail "fromLists: unconsistent data")
			let offset = 4 * i + j
			unsafeWrite cr (2 * offset) c
			unsafeWrite cr (2 * offset + 1) p

	cr' <- unsafeFreeze cr
	ls' <- unsafeFreeze ls
	st' <- unsafeFreeze st
	return $! Tangle
		{ count          = n
		, numberOfLegs   = l
		, crossingsArray = cr'
		, borderArray    = ls'
		, stateArray     = st' 
		}


toPair :: Dart ct -> (Int, Int)
toPair d
	| isLeg d    = (0, legPlace d)
	| otherwise  = (crossingIndex $ incidentCrossing d, dartPlace d)


toLists :: (CrossingType ct) => Tangle ct -> ([(Int, Int)], [([(Int, Int)], CrossingState ct)])
toLists tangle =
	let crToList c = (map (toPair . opposite) $ incidentDarts c, crossingState c)
	in (map (toPair . opposite) $ allLegs tangle, map crToList $ allCrossings tangle)
