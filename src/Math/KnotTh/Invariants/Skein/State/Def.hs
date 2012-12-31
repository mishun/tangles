module Math.KnotTh.Invariants.Skein.State.Def
	( SkeinState(..)
	, stateFromKnotted
	) where

import Data.STRef (STRef, newSTRef)
import Data.Array.MArray (newArray, newArray_, writeArray)
import Data.Array.ST (STUArray, STArray)
import Control.Monad.ST (ST)
import Control.Monad (forM_)
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Knotted
import Math.KnotTh.Invariants.Skein.Relation


data SkeinState s r a = SkeinState
	{ relation :: !r
	, size     :: !Int
	, alive    :: !(STRef s Int)
	, active   :: !(STUArray s Int Bool)
	, state    :: !(STArray s Int (StateSum a))
	, adjacent :: !(STArray s Int (STArray s Int (Int, Int)))
	, queue    :: !(STRef s [Int])
	, queued   :: !(STUArray s Int Bool)
	, multiple :: !(STRef s a)
	}


stateFromKnotted :: (SkeinRelation r a, SkeinKnotted k c d) => r -> k ArbitraryCrossing -> ST s (SkeinState s r a)
stateFromKnotted relation' knot = do
	let n = numberOfCrossings knot

	adjacent' <- newArray_ (0, n)
	ends <- newArray_ (0, numberOfEndpoints knot - 1)
	writeArray adjacent' 0 ends

	let pair d
		| isOverCrossing s  = p `seq` (i, p)
		| otherwise         = let p' = (p + 1) `mod` 4 in p' `seq` (i, p')
		where
			(c, p) = begin d
			s = crossingState c
			i = crossingIndex c

	forM_ (allCrossings knot) $ \ !c -> do
		let i = crossingIndex c
		writeArray adjacent' i =<< do
			x <- newArray_ (0, 3)
			forM_ (incidentDarts c) $ \ !a -> do
				let ap@(!_, !ai)  = pair a
				let b = opposite a
				if isDart b
					then writeArray x ai $! pair b
					else do
						let j = endpointPlace b
						writeArray x ai $ (0, j)
						writeArray ends j ap
			return $! x

	alive' <- newSTRef n
	active' <- newArray (1, n) True
	state' <- newArray (1, n) $! fromInitialSum $! initialLplus relation'
	queue' <- newSTRef [1 .. n]
	queued' <- newArray (1, n) True
	multiple' <- newSTRef 1
	return $! SkeinState
		{ relation = relation'
		, size     = n
		, alive    = alive'
		, active   = active'
		, state    = state'
		, adjacent = adjacent'
		, queue    = queue'
		, queued   = queued'
		, multiple = multiple'
		}

{-
copyState :: SkeinState s a -> ST s (SkeinState s a)
copyState s = do
	alive' <- newSTRef =<< readSTRef (alive s)
	active' <- mapArray id (active s)
	state' <- mapArray id (state s)

	(0, n) <- getBounds (adjacent s)
	adjacent' <- newArray_ (0, n)
	forM_ [0 .. n] $ \ i ->
		readArray (adjacent s) i >>= mapArray id >>= writeArray adjacent' i

	return $! SkeinState
		{ alive    = alive'
		, active   = active'
		, state    = state'
		, adjacent = adjacent'
		}
-}
{-
dumpState :: SkeinM s r a ()
dumpState = ask >>= \ s -> lift $ do
	str <- forM [1 .. size s] $ \ i -> do
		a <- readArray (active s) i
		adj <- if not a
			then return ""
			else do
				k <- readArray (adjacent s) i
				(_, d) <- getBounds k
				ss <- forM [0 .. d] $ \ j ->
					show `fmap` readArray k j
				return $ concat ss
		let x :: String
		    x = printf "%s { %s }" (show a) adj
		return x

	alive' <- readSTRef (alive s)

	let x = printf "\nalive = %i\n%s" alive' $ concatMap (++ "\n") str

	trace x $ return ()
-}
