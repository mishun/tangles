module Math.KnotTh.Invariants.Skein.State.RelaxVertex
	( contractLoopST
	) where

import Data.Array.Base ((!))
import Data.Array.MArray (newArray_, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Array.Unsafe (unsafeFreeze)
import Control.Monad.ST (ST)
import Control.Monad (forM_, when, foldM_)
import Text.Printf
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.State.Def
import Math.KnotTh.Invariants.Skein.State.Basic


contractLoopST :: (SkeinRelation r a) => (Int, Int) -> SkeinState s r a -> ST s ()
contractLoopST (!v, !p) s = do
	degree <- vertexDegreeST v s
	(!v', !p') <- neighbourST (v, p) s
	when (v' /= v || p' /= (p + 1) `mod` degree) $ fail $
		printf "contractLoopST: not loop (%i, %i) <-> (%i, %i) / %i" v p v' p' degree

	subst <- newArray_ (0, degree - 1) :: ST s (STUArray s Int Int)
	foldM_ (\ !k !i ->
		if i == p' || i == p
			then return $! k
			else writeArray subst i k >> (return $! k + 1)
		) 0 [0 .. degree - 1]

	do
		prev <- readArray (adjacent s) v
		next <- newArray_ (0, degree - 3)
		writeArray (adjacent s) v next

		forM_ [0 .. degree - 1] $ \ !i -> when (i /= p' && i /= p) $ do
			i' <- readArray subst i
			(u, j) <- readArray prev i
			if u /= v
				then connectST (v, i') (u, j) s
				else readArray subst j >>= \ j' -> connectST (v, i') (v, j') s

	readArray (state s) v >>= mapM (\ (StateSummand x k) -> do
		xm <- newArray_ (0, degree - 3) :: ST s (STUArray s Int Int)

		forM_ [0 .. degree - 1] $ \ !i -> when (i /= p' && i /= p) $ do
			let j | (x ! i) == p   = x ! p'
			      | (x ! i) == p'  = x ! p
			      | otherwise      = x ! i
			i' <- readArray subst i
			j' <- readArray subst j
			writeArray xm i' j'
			writeArray xm j' i'

		unsafeFreeze xm >>= \ x' ->
			return $! StateSummand x' $!
				if x ! p == p' then k * circleFactor (relation s) else k
		) >>= writeArray (state s) v . normalizeStateSum

	tryKillZeroVertexST v s
