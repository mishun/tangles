module Math.KnotTh.Invariants.Skein.SkeinM.Basic
	( appendMultipleST
	, extractMultipleST
	, connectST
	, vertexDegreeST
	, neighbourST
	, killVertexST
	, enqueueST
	, dequeueST
	, dissolveVertexST
	, numberOfAliveVerticesST
	, aliveVerticesST
	) where

import Data.STRef (readSTRef, writeSTRef)
import Data.Array.MArray (getBounds, readArray, writeArray)
import Control.Monad.ST (ST)
import Control.Monad (when, unless, filterM)
import Math.KnotTh.Invariants.Skein.StateSum
import Math.KnotTh.Invariants.Skein.SkeinM.Def


appendMultipleST :: (Num a) => SkeinState s r a -> a -> ST s ()
appendMultipleST s x =
	readSTRef (multiple s) >>= \ !m ->
		writeSTRef (multiple s) $! x * m


extractMultipleST :: SkeinState s r a -> ST s a
extractMultipleST s = readSTRef $ multiple s


connectST :: SkeinState s r a -> (Int, Int) -> (Int, Int) -> ST s ()
connectST s a@(!v, !p) b@(!u, !q) = do
	readArray (adjacent s) v >>= \ d -> writeArray d p b
	readArray (adjacent s) u >>= \ d -> writeArray d q a


vertexDegreeST :: SkeinState s r a -> Int -> ST s Int
vertexDegreeST s v = do
	readArray (adjacent s) v >>=
		getBounds >>= \ (0, n) ->
			return $! n + 1


neighbourST :: SkeinState s r a -> (Int, Int) -> ST s (Int, Int)
neighbourST s (!v, !p) = do
	x <- readArray (adjacent s) v
	(_, d) <- getBounds x
	readArray x $ p `mod` (d + 1)


killVertexST :: SkeinState s r a -> Int -> ST s ()
killVertexST s v = do
	a <- readArray (active s) v
	unless a $ fail "killVertexST: vertex is already dead"
	writeArray (active s) v False
	writeArray (state s) v $ error "do not touch!"
	writeArray (adjacent s) v $ error "do not touch!"
	readSTRef (alive s) >>= \ !x ->
		writeSTRef (alive s) $! x - 1


enqueueST :: SkeinState s r a -> Int -> ST s ()
enqueueST s v = do
	a <- readArray (active s) v
	e <- readArray (queued s) v
	when (a && not e) $ do
		writeArray (queued s) v True
		readSTRef (queue s) >>= \ !l -> writeSTRef (queue s) $! v : l


dequeueST :: SkeinState s r a -> ST s (Maybe Int)
dequeueST s = do
	l <- readSTRef $ queue s
	case l of
		[]    -> return Nothing
		h : t -> do
			writeSTRef (queue s) t
			writeArray (queued s) h False
			ok <- readArray (active s) h
			if ok
				then return $! Just $! h
				else dequeueST s


dissolveVertexST :: (Num a) => SkeinState s r a -> Int -> ST s ()
dissolveVertexST s v = do
	stateSum <- readArray (state s) v
	case stateSum of
		[]                 -> appendMultipleST s 0
		[StateSummand _ x] -> appendMultipleST s x
		_                  -> fail "internal error: zero degree vertex and StateSum with length > 1"
	killVertexST s v


numberOfAliveVerticesST :: SkeinState s r a -> ST s Int
numberOfAliveVerticesST s = readSTRef $ alive s


aliveVerticesST :: SkeinState s r a -> ST s [Int]
aliveVerticesST s = filterM (readArray $ active s) [1 .. size s]
