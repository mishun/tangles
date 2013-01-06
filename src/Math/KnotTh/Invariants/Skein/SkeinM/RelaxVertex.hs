module Math.KnotTh.Invariants.Skein.SkeinM.RelaxVertex
	( tryRelaxVertex
	) where

import Data.Array.Base ((!), bounds)
import Data.Array.Unboxed (UArray)
import Data.Array.MArray (newArray_, readArray, writeArray)
import Data.Array.ST (STUArray, runSTUArray)
import Control.Monad.ST (ST)
import Control.Monad (forM_, when, foldM_)
import Text.Printf
import Math.KnotTh.Invariants.Skein.StateSum
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.SkeinM.Def (SkeinState, relation)
import Math.KnotTh.Invariants.Skein.SkeinM.Basic


tryRelaxVertex :: (SkeinRelation r a) => SkeinState s r a -> Int -> ST s Bool
tryRelaxVertex s v = do
	degree <- vertexDegreeST s v
	case degree of
		0 -> do
			dissolveVertexST s v
			return True

		2 -> do
			a@(av, _) <- neighbourST s (v, 0)
			b@(bv, _) <- neighbourST s (v, 1)
			dissolveVertexST s v
			if a == (v, 1)
				then appendMultipleST s $ circleFactor $ relation s
				else connectST s a b >> enqueueST s av >> enqueueST s bv
			return True

		_ -> do
			let findLoop [] = return False
			    findLoop (i : t) = do
			    	(u, j) <- neighbourST s (v, i)
			    	if u /= v || j /= (i + 1) `mod` degree
			    		then findLoop t
			    		else do
			    			contractLoopST s (v, i)
			    			enqueueST s v
			    			return True
			findLoop [0 .. degree - 1]


contractLoopST :: (SkeinRelation r a) => SkeinState s r a -> (Int, Int) -> ST s ()
contractLoopST s (!v, !p) = do
	degree <- vertexDegreeST s v
	(!v', !p') <- neighbourST s (v, p)
	when (v' /= v || p' /= (p + 1) `mod` degree) $ fail $
		printf "contractLoopST: not loop (%i, %i) <-> (%i, %i) / %i" v p v' p' degree

	let subst = runSTUArray $ do
		a <- newArray_ (0, degree - 1) :: ST s (STUArray s Int Int)
		foldM_ (\ !k !i ->
			if i == p' || i == p
				then return $! k
				else writeArray a i k >> (return $! k + 1)
			) 0 [0 .. degree - 1]
		return $! a

	prev <- resizeAdjListST s v $ degree - 2
	forM_ [0 .. degree - 1] $ \ !i -> when (i /= p' && i /= p) $ do
		(u, j) <- readArray prev i
		connectST s (v, subst ! i) $ if u /= v then (u, j) else (v, subst ! j)

	modifyStateSumST s v $ glue (relation s) p p' subst


glue :: (SkeinRelation r a) => r -> Int -> Int -> UArray Int Int -> [StateSummand a] -> [StateSummand a]
glue rel p p' subst preSum = normalizeStateSum $ do
	StateSummand x k <- preSum

	let x' = runSTUArray $ do
		let (0, bound) = bounds subst
		xm <- newArray_ (0, bound - 2) :: ST s (STUArray s Int Int)
		forM_ [0 .. bound] $ \ !i -> when (i /= p' && i /= p) $ do
			let j | (x ! i) == p   = x ! p'
			      | (x ! i) == p'  = x ! p
			      | otherwise      = x ! i
			writeArray xm (subst ! i) (subst ! j)
		return $! xm

	let k' | x ! p == p'                    = k * circleFactor rel
	       | cross (x ! p, p) (x ! p', p')  = k * (if min p' (x ! p') < min p (x ! p) then twistPFactor else twistNFactor) rel
	       | otherwise                      = k

	return $! StateSummand x' k'
	where
		cross (a, b) (c, d) =
			let a' = min a b ; b' = max a b
			    c' = min c d ; d' = max c d
			in (a' < c' && b' > c' && b' < d') || (a' > c' && a' < d' && b' > d')
