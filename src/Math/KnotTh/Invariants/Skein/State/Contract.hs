module Math.KnotTh.Invariants.Skein.State.Contract
	( contractEdgeST
	) where

import Data.Array.Base ((!))
import Data.Array.MArray (newArray_, readArray, writeArray)
import Data.Array.ST (STUArray)
import Data.Array.Unsafe (unsafeFreeze)
import Control.Monad.ST (ST)
import Control.Monad (forM_, forM, when)
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.State.Def
import Math.KnotTh.Invariants.Skein.State.Basic


contractEdgeST :: (SkeinRelation r a) => (Int, Int) -> SkeinState s r a -> ST s ()
contractEdgeST (!v, !p) s = do
	(!u, !q) <- neighbourST (v, p) s
	when (v == u) $ fail "contractEdgeST: not edge"

	degreeV <- vertexDegreeST v s
	substV <- newArray_ (0, degreeV - 1) :: ST s (STUArray s Int Int)
	degreeU <- vertexDegreeST u s
	substU <- newArray_ (0, degreeU - 1) :: ST s (STUArray s Int Int)

	forM_ [0 .. p - 1] $ \ !i -> writeArray substV i i
	forM_ [q + 1 .. degreeU - 1] $ \ !i -> writeArray substU i $ i - q - 1 + p
	forM_ [0 .. q - 1] $ \ !i -> writeArray substU i $ i + degreeU - q + p - 1
	forM_ [p + 1 .. degreeV - 1] $ \ !i -> writeArray substV i $ i + degreeU - 2

	--getElems substV >>= \ x -> trace (show x) (return ())
	--getElems substU >>= \ x -> trace (show x) (return ())

	do
		prevV <- readArray (adjacent s) v
		prevU <- readArray (adjacent s) u
		next <- newArray_ (0, degreeV + degreeU - 3)
		writeArray (adjacent s) v next

		forM_ [0 .. degreeV - 1] $ \ !i -> when (i /= p) $ do
			i' <- readArray substV i
			(w, k) <- readArray prevV i
			case () of
				_ | w == v    -> readArray substV k >>= \ k' -> connectST (v, i') (v, k') s
				  | w == u    -> readArray substU k >>= \ k' -> connectST (v, i') (v, k') s
				  | otherwise -> connectST (v, i') (w, k) s

		forM_ [0 .. degreeU - 1] $ \ !i -> when (i /= q) $ do
			i' <- readArray substU i
			(w, k) <- readArray prevU i
			case () of
				_ | w == v    -> readArray substV k >>= \ k' -> connectST (v, i') (v, k') s
				  | w == u    -> readArray substU k >>= \ k' -> connectST (v, i') (v, k') s
				  | otherwise -> connectST (v, i') (w, k) s

	do
		sumV <- readArray (state s) v
		sumU <- readArray (state s) u

		(writeArray (state s) v . normalizeStateSum =<<) $
			forM [(a, b) | a <- sumV, b <- sumU] $ \ (StateSummand !xa !ka, StateSummand !xb !kb) -> do
				xm <- newArray_ (0, degreeV + degreeU - 3) :: ST s (STUArray s Int Int)

				forM_ [0 .. degreeV - 1] $ \ !i -> when (i /= p) $ do
					i' <- readArray substV i
					j' <- if (xa ! i) == p
						then readArray substU (xb ! q)
						else readArray substV (xa ! i)
					writeArray xm i' j'
					writeArray xm j' i'

				forM_ [0 .. degreeU - 1] $ \ !i -> when (i /= q) $ do
					i' <- readArray substU i
					j' <- if (xb ! i) == q
						then readArray substV (xa ! p)
						else readArray substU (xb ! i)
					writeArray xm i' j'
					writeArray xm j' i'

				unsafeFreeze xm >>= \ x' ->
					return $! StateSummand x' (ka * kb)

	killVertexST u s
	tryKillZeroVertexST v s
