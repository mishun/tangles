module Math.KnotTh.Invariants.Skein.SkeinM.Basic
	( appendMultipleST
	, extractMultipleST
	, connectST
	, vertexDegreeST
	, neighbourST
	, killVertexST
	, enqueueST
	, dequeueST
	, numberOfAliveVerticesST
	, aliveVerticesST
	) where

import Data.STRef (readSTRef, writeSTRef)
import Data.Array.MArray (getBounds, readArray, writeArray)
import Control.Monad.ST (ST)
import Control.Monad (when, unless, filterM)
import Math.KnotTh.Invariants.Skein.SkeinM.Def


appendMultipleST :: (Num a) => a -> SkeinState s r a -> ST s ()
appendMultipleST x s =
	readSTRef (multiple s) >>= \ !m ->
		writeSTRef (multiple s) $! x * m


extractMultipleST :: SkeinState s r a -> ST s a
extractMultipleST s = readSTRef $ multiple s


connectST :: (Int, Int) -> (Int, Int) -> SkeinState s r a -> ST s ()
connectST a@(!v, !p) b@(!u, !q) s = do
	readArray (adjacent s) v >>= \ d -> writeArray d p b
	readArray (adjacent s) u >>= \ d -> writeArray d q a


vertexDegreeST :: Int -> SkeinState s r a ->  ST s Int
vertexDegreeST v s = do
	readArray (adjacent s) v >>=
		getBounds >>= \ (0, n) ->
			return $! n + 1


neighbourST :: (Int, Int) -> SkeinState s r a -> ST s (Int, Int)
neighbourST (!v, !p) s = do
	x <- readArray (adjacent s) v
	(_, d) <- getBounds x
	readArray x $ p `mod` (d + 1)


killVertexST :: Int -> SkeinState s r a -> ST s ()
killVertexST v s = do
	a <- readArray (active s) v
	unless a $ fail "killVertexST: vertex is already dead"
	writeArray (active s) v False
	writeArray (state s) v $ error "do not touch!"
	writeArray (adjacent s) v $ error "do not touch!"
	readSTRef (alive s) >>= \ !x ->
		writeSTRef (alive s) $! x - 1


enqueueST :: Int -> SkeinState s r a -> ST s ()
enqueueST v s = do
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


numberOfAliveVerticesST :: SkeinState s r a -> ST s Int
numberOfAliveVerticesST s = readSTRef $ alive s


aliveVerticesST :: SkeinState s r a -> ST s [Int]
aliveVerticesST s = filterM (readArray $ active s) [1 .. size s]
