module Math.KnotTh.Invariants.Skein.SkeinM.Def
	( SkeinState(..)
	, stateFromKnotted
	, dumpStateST
	) where

import Data.STRef (STRef, newSTRef, readSTRef)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray, getBounds)
import Data.Array.ST (STUArray, STArray)
import Control.Monad.ST (ST)
import Control.Monad (forM, forM_)
import Text.Printf
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Knotted
import Math.KnotTh.Invariants.Skein.StateSum
import Math.KnotTh.Invariants.Skein.Knotted
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


stateFromKnotted :: (SkeinRelation rel a, SkeinKnotted k c d) => rel -> k ArbitraryCrossing -> ST s (SkeinState s rel a)
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
	multiple' <- newSTRef $ circleFactor relation' ^ numberOfFreeLoops knot
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
	let n = size s

	alive' <- newSTRef =<< readSTRef (alive s)
	active' <- mapArray id (active s)
	state' <- mapArray id (state s)

	adjacent' <- newArray_ (0, n)
	forM_ [0 .. n] $ \ i ->
		readArray (adjacent s) i >>= mapArray id >>= writeArray adjacent' i

	return $! SkeinState
		{ relation = relation s
		, size     = n
		, alive    = alive'
		, active   = active'
		, state    = state'
		, adjacent = adjacent'
		}
-}

dumpStateST :: (Show a) => SkeinState s r a -> ST s String
dumpStateST s = do
	cross <- forM [1 .. size s] $ \ i -> do
		act <- readArray (active s) i
		if not act
			then return "???"
			else do
				adj <- readArray (adjacent s) i
				(0, bound) <- getBounds adj
				con <- fmap concat $ forM [0 .. bound] $ \ j -> show `fmap` readArray adj j
				st <- readArray (state s) i
				return $ printf "{ %s } %s" con (show st)

	alive' <- readSTRef $ alive s
	multiple' <- readSTRef $ multiple s
	return $! printf "\nalive = %i\nmultiple=%s\n%s" alive' (show multiple') $ concatMap (++ "\n") cross
