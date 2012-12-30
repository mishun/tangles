{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Invariants.Skein.State
	( Vertex
	, SkeinM
	, enqueue
	, dequeue
	, vertexDegree
	, neighbour
	, aliveVertices
	, contract
	, runSkein
	) where

import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Data.Array.MArray (getBounds, newArray, newArray_, readArray, writeArray)
import Data.Array.ST (STUArray, STArray)
import Control.Monad.ST (ST, runST)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad (forM_, when, filterM)
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Knotted
import Math.KnotTh.Invariants.Skein.Relation


newtype Vertex = Vertex Int


data SkeinState s a = SkeinState
	{ size     :: !Int
	, alive    :: !(STRef s Int)
	, active   :: !(STUArray s Int Bool)
	, state    :: !(STArray s Int (StateSum a))
	, adjacent :: !(STArray s Int (STArray s Int (Int, Int)))
	, queue    :: !(STRef s [Vertex])
	, queued   :: !(STUArray s Int Bool)
	}


stateFromKnotted :: (SkeinRelation r a, SkeinKnotted k c d) => r -> k ArbitraryCrossing -> ST s (SkeinState s a)
stateFromKnotted relation knot = do
	let n = numberOfCrossings knot

	adjacent' <- newArray_ (0, n)
	ends <- newArray_ (0, numberOfEndpoints knot - 1)
	writeArray adjacent' 0 ends

	let pair d
		| isOverCrossing c  = p `seq` (0, p)
		| otherwise         = let p' = (p + 1) `mod` 4 in p' `seq` (0, p')
		where
			c = crossingState $ incidentCrossing d
			p = dartPlace d

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
	state' <- newArray (1, n) $! initialLplus relation
	queue' <- newSTRef $! map Vertex [1 .. n]
	queued' <- newArray (1, n) True
	return $! SkeinState
		{ size     = n
		, alive    = alive'
		, active   = active'
		, state    = state'
		, adjacent = adjacent'
		, queue    = queue'
		, queued   = queued'
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


type SkeinM s a = ReaderT (SkeinState s a) (ST s)


enqueue :: Vertex -> SkeinM s a Bool
enqueue (Vertex v) = ask >>= \ !s -> lift $ do
	a <- readArray (active s) v
	e <- readArray (queued s) v
	if not a || e
		then return False
		else do
			writeArray (queued s) v True
			readSTRef (queue s) >>= \ !l ->
				writeSTRef (queue s) $! Vertex v : l
			return True


dequeue :: SkeinM s a (Maybe Vertex)
dequeue = do
	ask >>= \ !s -> lift $ do
		return undefined


vertexDegree :: Vertex -> SkeinM s a Int
vertexDegree (Vertex !v) = ask >>= \ !s -> lift $ do
	readArray (adjacent s) v >>=
		getBounds >>= \ (0, n) -> return $! n + 1


neighbour :: (Vertex, Int) -> SkeinM s a (Vertex, Int)
neighbour (Vertex !v, p) = ask >>= \ !s -> lift $ do
	x <- readArray (adjacent s) v
	(_, d) <- getBounds x
	(!u, !q) <- readArray x $ p `mod` (d + 1)
	when (u == 0) $ fail "touching border"
	return $! (Vertex u, q)


aliveVertices :: SkeinM s a [Vertex]
aliveVertices = ask >>= \ !s -> lift $ do
	map Vertex `fmap` filterM (readArray $ active s) [1 .. size s]


contract :: (Vertex, Int) -> SkeinM s a Vertex
contract (vertex@(Vertex !v), !p') = do
	degree <- vertexDegree vertex
	ask >>= \ !s -> lift $ do
		when (degree == 0) $ fail "trying to contract vertex with degree 0"
		let p = p' `mod` degree
		(!u, !q) <- readArray (adjacent s) v >>= flip readArray p
		when (u == 0) $ fail "trying to contract endpoint"
		if u == v
			then if q == (p + 1) `mod` degree
				then return undefined
				else fail "contraction with itself is possible only for simple loop"
			else return undefined


runSkein :: (SkeinRelation r a, SkeinKnotted k c d) => r -> (forall s. SkeinM s a ()) -> k ArbitraryCrossing -> StateSum a
runSkein relation action knot = runST $ do
	when (numberOfCrossings knot == 0) $ fail "runSken: no crossings in knot"
	(stateFromKnotted relation knot >>=) $ runReaderT $ do
		action
		[Vertex v] <- aliveVertices
		ask >>= \ !s -> lift $
			readArray (state s) v
