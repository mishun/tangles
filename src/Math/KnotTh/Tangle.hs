{-# LANGUAGE TemplateHaskell #-}
module Math.KnotTh.Tangle
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
	, allLegOpposites
	, isAdjacentToBorder
	, maybeIncidentCrossing
	, maybeAdjacentCrossing
	, allLegsAndDarts
	, allEdges
	, lonerTangle
	, transformTangle
	, glueToBorder
	, implode
	, explode
	, containingDirectedPath
	, containingUndirectedPath
	, directedPathsDecomposition
	, undirectedPathsDecomposition
	, containingThread
	, allThreads
	, containingFaceLeft
	, containingFaceRight
	, allTangleFaces
	) where

import Data.List (intercalate, nub, foldl', sort)
import qualified Data.Set as Set
import qualified Data.Map as Map
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
import Text.Printf (printf)
import Math.Algebra.RotationDirection
import Math.Algebra.Group.Dn (Dn, pointsUnderGroup, reflection, rotation, permute)
import Math.Algebra.Group.D4 ((<*>), ec)
import Math.KnotTh.Knotted
import Math.KnotTh.Knotted.TH.Show


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
		| isLeg d    = printf "(Leg %i)" (legPlace d)
		| otherwise  = let (c, p) = toPair d in printf "(Dart %i %i)" c p


data Crossing ct = Crossing !(Tangle ct) {-# UNPACK #-} !Int

instance Eq (Crossing ct) where
	(==) (Crossing _ c1) (Crossing _ c2) = (c1 == c2)

instance Ord (Crossing ct) where
	compare (Crossing _ c1) (Crossing _ c2) = compare c1 c2


data Tangle ct = Tangle
	{ crossCount     :: {-# UNPACK #-} !Int
	, numberOfLegs   :: {-# UNPACK #-} !Int
	, crossingsArray :: {-# UNPACK #-} !(UArray Int Int)
	, borderArray    :: {-# UNPACK #-} !(UArray Int Int)
	, stateArray     :: {-# UNPACK #-} !(Array Int (CrossingState ct))
	, loopsCount     :: {-# UNPACK #-} !Int
	}


instance (NFData ct) => NFData (Tangle ct) where
	rnf tangle = rnf (stateArray tangle) `seq` tangle `seq` ()


instance (Show ct, CrossingType ct) => Show (Tangle ct) where
	show tangle =
		printf "(Tangle (%i O) (Border [ %s ]) %s)"
			(numberOfFreeLoops tangle)
			(intercalate " " $ (map show $ allLegOpposites tangle))
			(intercalate " " $ map show $ allCrossings tangle)


instance Knotted Tangle Crossing Dart where
	numberOfFreeLoops = loopsCount

	numberOfCrossings = crossCount

	numberOfEdges tangle = 2 * (numberOfCrossings tangle) + div (numberOfLegs tangle) 2

	nthCrossing t i
		| i < 1 || i > numberOfCrossings t  = error "nthCrossing: out of bound"
		| otherwise                         = Crossing t i

	mapCrossings f tangle = tangle
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


instance KnottedWithConnectivity Tangle Crossing Dart where
	isConnected tangle
		| numberOfFreeLoops tangle /= 0  = False
		| otherwise                      = all (\ (a, b) -> Set.member a con && Set.member b con) edges
		where
			edges = allEdges tangle
			con = dfs (Set.empty) $ fst $ head edges
			dfs vis c
				| Set.member c vis  = vis
				| otherwise         = foldl' dfs (Set.insert c vis) neigh
				where
					neigh
						| isLeg c    = [opposite c]
						| otherwise  = [opposite c, nextCCW c, nextCW c]

	isPrime tangle = connections == nub connections
		where
			idm =	let faces = allTangleFaces tangle
				in Map.fromList $ concatMap (\ (face, i) -> zip face $ repeat i) $ zip faces [(0 :: Int) ..]

			connections = sort $ map getPair $ allEdges tangle
				where
					getPair (da, db) = (min a b, max a b)
						where
							a = idm Map.! da
							b = idm Map.! db


instance KnottedWithAccel Tangle Crossing Dart where
	forMIncidentDarts (Crossing t c) f = do
		f $! Dart t c 0
		f $! Dart t c 1
		f $! Dart t c 2
		f $! Dart t c 3

	foldMIncidentDarts (Crossing t c) f s =
		f (Dart t c 0) s >>= f (Dart t c 1) >>= f (Dart t c 2) >>= f (Dart t c 3)

	foldMIncidentDartsFrom dart@(Dart t c i) !dir f s
		| isLeg dart  = error "foldMIncidentDartsFrom: from leg"
		| otherwise   =
			let d = directionSign dir
			in f dart s >>= f (Dart t c $! (i + d) .&. 3)
				>>= f (Dart t c $! (i + 2 * d) .&. 3) >>= f (Dart t c $! (i + 3 * d) .&. 3)


instance KnottedWithToPair Tangle Crossing Dart where
	toPair d
		| isLeg d    =
			let p = legPlace d
			in p `seq` (0, p)
		| otherwise  =
			let c = crossingIndex $ incidentCrossing d
			    p = dartPlace d
			in c `seq` p `seq` (c, p)


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
nthLeg t i = Dart t 0 (mod i $ numberOfLegs t)


{-# INLINE firstLeg #-}
firstLeg :: Tangle ct -> Dart ct
firstLeg t = Dart t 0 0


{-# INLINE allLegs #-}
allLegs :: Tangle ct -> [Dart ct]
allLegs t = map (Dart t 0) [0 .. numberOfLegs t - 1]


{-# INLINE allLegOpposites #-}
allLegOpposites :: Tangle ct -> [Dart ct]
allLegOpposites = map opposite . allLegs


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
	{ crossCount     = 1
	, numberOfLegs   = 4
	, crossingsArray = listArray (0, 7) [0, 0, 0, 1, 0, 2, 0, 3]
	, borderArray    = listArray (0, 7) [1, 0, 1, 1, 1, 2, 1, 3]
	, stateArray     = listArray (0, 0) $! [cr]
	, loopsCount     = 0
	}


transformTangle :: (CrossingType ct) => Dn -> Tangle ct -> Tangle ct
transformTangle g tangle
	| l /= pointsUnderGroup g                   = error "transformTangle: order conflict"
	| reflection g == False && rotation g == 0  = tangle
	| otherwise                                 = implode (numberOfFreeLoops tangle, border, map crossing $ allCrossings tangle)
	where
		l = numberOfLegs tangle

		pair d
			| isLeg d    = (0, permute g $ legPlace d)
			| otherwise  =
				let c = incidentCrossing d
				in (crossingIndex c, if reflection g then 3 - dartPlace d else dartPlace d)

		crossing c
			| reflection g  = (reverse $ map pair $ adjacentDarts c, mapOrientation (ec <*>) $ crossingState c)
			| otherwise     = (map pair $ adjacentDarts c, crossingState c)

		border
			| reflection g  = head rotated : reverse (tail rotated)
			| otherwise     = rotated
			where
				rotated =
					let (pre, post) = splitAt (l - rotation g) $ map (pair . opposite) $ allLegs tangle
					in post ++ pre


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
glueToBorder leg legsToGlue crossingToGlue
	| not (isLeg leg)                   = error "glueToBorder: leg expected"
	| legsToGlue < 1 || legsToGlue > 3  = error "glueToBorder: legsToGlue must be 1, 2 or 3"
	| otherwise                         = runST $ do
		let tangle = dartTangle leg
		let oldL = numberOfLegs tangle
		when (oldL <= legsToGlue) (fail "glueToBorder: not enough legs to glue")

		let oldC = numberOfCrossings tangle
		let newC = oldC + 1
		let newL = oldL + 4 - 2 * legsToGlue
		let lp = legPlace leg

		cr <- newArray_ (0, 8 * newC - 1)
		ls <- newArray_ (0, 2 * newL - 1)


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
					let ml = (p - lp - 1) `mod` oldL
					if ml < oldL - legsToGlue
						then writePair arr index 0 (4 - legsToGlue + ml)
						else writePair arr index newC (oldL - 1 - ml)
				else writePair arr index c p

		forM_ [0 .. 4 * oldC - 1] $ \ !i ->
			copyModified cr i (crossingsArray tangle) i

		forM_ [0 .. legsToGlue - 1] $ \ !i ->
			copyModified cr (4 * (newC - 1) + i) (borderArray tangle) ((lp - i) `mod` oldL)

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
			{ crossCount     = newC
			, numberOfLegs   = newL
			, crossingsArray = cr'
			, borderArray    = ls'
			, stateArray     = st' 
			, loopsCount     = numberOfFreeLoops tangle
			}

		return $! nthCrossing result newC


implode :: (Int, [(Int, Int)], [([(Int, Int)], CrossingState ct)]) -> Tangle ct
implode (!loops, !border, !list) = runST $ do
	when (loops < 0) (fail "fromListST: number of free loops is negative")

	let n = length list
	let l = length border
	when (l <= 0 || odd l) (fail "fromLists: number of legs must be positive and even")

	let	{-# INLINE testPair #-}
		testPair c p =
			case c of
				0                  -> when (p < 0 || p >= l) (fail "fromLists: leg index is out of bound")
				_ | c < 0 || c > n -> fail "fromLists: crossing index must be from 1 to number of crossings"
				  | otherwise      -> when (p < 0 || p > 3) (fail "fromLists: place index is out of bound")

	ls <- newArray_ (0, 2 * l - 1)
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
				let (a, offset)
					| c == 0     = (ls, p)
					| otherwise  = (cr, 4 * (c - 1) + p)
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
		{ crossCount     = n
		, numberOfLegs   = l
		, crossingsArray = cr'
		, borderArray    = ls'
		, stateArray     = st' 
		, loopsCount     = loops
		}


explode :: (CrossingType ct) => Tangle ct -> (Int, [(Int, Int)], [([(Int, Int)], CrossingState ct)])
explode tangle =
	let crToList c = (map (toPair . opposite) $ incidentDarts c, crossingState c)
	in (numberOfFreeLoops tangle, map (toPair . opposite) $ allLegs tangle, map crToList $ allCrossings tangle)


containingDirectedPath :: (Dart ct -> Dart ct, Dart ct -> Dart ct) -> Dart ct -> [Dart ct]
containingDirectedPath (adjForward, adjBackward) start
	| isCycle    = forward
	| otherwise  = walkBackward (start, forward)
	where
		(forward, isCycle) = walkForward start

		walkForward d
			| isLeg opp     = ([d], False)
			| start == nxt  = ([d], True)
			| otherwise     = (d : nextPath, nextCycle)
			where
				opp = opposite d
				nxt = adjForward opp
				(nextPath, nextCycle) = walkForward nxt

		walkBackward (d, path)
			| isLeg d    = path
			| otherwise  = let prev = opposite $ adjBackward d in walkBackward (prev, prev : path)


containingUndirectedPath :: (Dart ct -> Dart ct) -> Dart ct -> [(Dart ct, Dart ct)]
containingUndirectedPath cont = map (\ d -> (d, opposite d)) . containingDirectedPath (cont, cont)


directedPathsDecomposition :: (Dart ct -> Dart ct, Dart ct -> Dart ct) -> Tangle ct -> [[Dart ct]]
directedPathsDecomposition continue = fst . foldl' processDart ([], Set.empty) . allLegsAndDarts
	where
		processDart (paths, s) d
			| Set.member d s  = (paths, s)
			| otherwise       = (path : paths, nextS)
			where
				path = containingDirectedPath continue d
				nextS = foldl' (\ curs a -> Set.insert a curs) s path


undirectedPathsDecomposition :: (Dart ct -> Dart ct) -> Tangle ct -> [[(Dart ct, Dart ct)]]
undirectedPathsDecomposition continue = fst . foldl' processDart ([], Set.empty) . allLegsAndDarts
	where
		processDart (!paths, s) d
			| Set.member d s  = (paths, s)
			| otherwise       = (path : paths, nextS)
			where
				path = containingUndirectedPath continue d
				nextS = foldl' (\ curs (a, b) -> Set.insert b $ Set.insert a curs) s path


containingThread :: Dart ct -> [(Dart ct, Dart ct)]
containingThread = containingUndirectedPath continuation


allThreads :: Tangle ct -> [[(Dart ct, Dart ct)]]
allThreads = undirectedPathsDecomposition continuation


containingFaceLeft :: Dart ct -> [Dart ct]
containingFaceLeft = containingDirectedPath (nextCW, nextCCW)


containingFaceRight :: Dart ct -> [Dart ct]
containingFaceRight = containingDirectedPath (nextCCW, nextCW)


allTangleFaces :: Tangle ct -> [[Dart ct]]
allTangleFaces = directedPathsDecomposition (nextCW, nextCCW)


$((:[]) `fmap` produceShowCrossing ''Crossing)
