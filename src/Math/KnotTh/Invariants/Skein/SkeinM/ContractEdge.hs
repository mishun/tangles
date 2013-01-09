module Math.KnotTh.Invariants.Skein.SkeinM.ContractEdge
	( contractEdgeST
	) where

import Data.Array.Base (bounds, (!))
import Data.Array.Unboxed (UArray)
import Data.Array.MArray (newArray_, readArray, writeArray)
import Data.Array.ST (STUArray, runSTUArray)
import Control.Monad.ST (ST)
import Control.Monad (forM_, when)
import Text.Printf
import Math.KnotTh.Invariants.Skein.StateSum
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.SkeinM.State


contractEdgeST :: (SkeinRelation r a) => SkeinState s r a -> (Int, Int) -> ST s ()
contractEdgeST s (!v, !p) = do
	(!u, !q) <- neighbourST s (v, p)
	when (v == 0 || u == 0 || v == u) $ fail $
		printf "contract: can not contract (%i, %i) <-> (%i, %i)" v p u q

	degreeV <- vertexDegreeST s v
	degreeU <- vertexDegreeST s u

	w <- if degreeV <= degreeU
		then contract s (v, p) (u, q)
		else contract s (u, q) (v, p)

	enqueueST s w


contract :: (SkeinRelation r a) => SkeinState s r a -> (Int, Int) -> (Int, Int) -> ST s Int
contract s (!v, !p) (!u, !q) = do
	degreeV <- vertexDegreeST s v
	degreeU <- vertexDegreeST s u

	let substV = runSTUArray $ do
		a <- newArray_ (0, degreeV - 1) :: ST s (STUArray s Int Int)
		forM_ [0 .. p - 1] $ \ !i -> writeArray a i i
		forM_ [p + 1 .. degreeV - 1] $ \ !i -> writeArray a i $ i + degreeU - 2
		return $! a

	let substU = runSTUArray $ do
		a <- newArray_ (0, degreeU - 1) :: ST s (STUArray s Int Int)
		forM_ [q + 1 .. degreeU - 1] $ \ !i -> writeArray a i $ i - q - 1 + p
		forM_ [0 .. q - 1] $ \ !i -> writeArray a i $ i + degreeU - q + p - 1
		return $! a

	do
		prevV <- resizeAdjListST s v $ degreeV + degreeU - 2
		prevU <- getAdjListST s u

		forM_ [0 .. degreeV - 1] $ \ !i -> when (i /= p) $ do
			(w, k) <- readArray prevV i
			connectST s (v, substV ! i) $ case () of
				_ | w == v    -> (v, substV ! k)
				  | w == u    -> (v, substU ! k)
				  | otherwise -> (w, k)

		forM_ [0 .. degreeU - 1] $ \ !i -> when (i /= q) $ do
			(w, k) <- readArray prevU i
			connectST s (v, substU ! i) $ case () of
				_ | w == v    -> (v, substV ! k)
				  | w == u    -> (v, substU ! k)
				  | otherwise -> (w, k)

	getStateSumST s u >>= \ sumU ->
		modifyStateSumST s v $ \ sumV ->
			glue (relation s) p substV sumV q substU sumU

	killVertexST s u
	return $! v


glue :: (SkeinRelation r a) => r -> Int -> UArray Int Int -> [StateSummand a] -> Int -> UArray Int Int -> [StateSummand a] -> [StateSummand a]
glue _ p substV sumV q substU sumU = normalizeStateSum $ do
	StateSummand xa ka <- sumV
	StateSummand xb kb <- sumU

	let x = runSTUArray $ do
		let (0, boundV) = bounds xa
		let (0, boundU) = bounds xb
		xm <- newArray_ (0, boundV + boundU - 1) :: ST s (STUArray s Int Int)

		forM_ [0 .. boundV] $ \ !i -> when (i /= p) $
			writeArray xm (substV ! i) $
				if (xa ! i) == p
					then substU ! (xb ! q)
					else substV ! (xa ! i)

		forM_ [0 .. boundU] $ \ !i -> when (i /= q) $
			writeArray xm (substU ! i) $
				if (xb ! i) == q
					then substV ! (xa ! p)
					else substU ! (xb ! i)

		return $! xm

	return $! StateSummand x (ka * kb)
