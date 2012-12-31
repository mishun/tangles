{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Invariants.Skein.State
	( SkeinM
	, Vertex
	, enqueue
	, dequeue
	, vertexDegree
	, neighbour
	, numberOfAliveVertices
	, contract
	, runSkein
	) where

import Data.Function (fix)
import Control.Monad.ST (ST, runST)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad (when)
import Text.Printf
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.State.Def
import Math.KnotTh.Invariants.Skein.State.Basic
import Math.KnotTh.Invariants.Skein.State.RelaxVertex
import Math.KnotTh.Invariants.Skein.State.Contract


type SkeinM s r a = ReaderT (SkeinState s r a) (ST s)


newtype Vertex = Vertex Int deriving (Eq)


enqueue :: Vertex -> SkeinM s r a ()
enqueue (Vertex v) = ask >>= lift . enqueueST v


dequeue :: SkeinM s r a (Maybe Vertex)
dequeue = ask >>= \ s -> lift $ do
	v <- dequeueST s
	return $! Vertex `fmap` v


vertexDegree :: Vertex -> SkeinM s r a Int
vertexDegree (Vertex v) = ask >>= lift . vertexDegreeST v


neighbour :: (Vertex, Int) -> SkeinM s r a (Vertex, Int)
neighbour (Vertex !v, p) = ask >>= \ !s -> lift $ do
	(!u, !q) <- neighbourST (v, p) s
	when (u == 0) $ fail "touching border"
	return $! (Vertex u, q)


aliveVertices :: SkeinM s r a [Vertex]
aliveVertices = ask >>= \ !s -> lift $ do
	l <- aliveVerticesST s
	return $! map Vertex l


numberOfAliveVertices :: SkeinM s r a Int
numberOfAliveVertices = ask >>= lift . numberOfAliveVerticesST


contract :: (SkeinRelation r a) => (Vertex, Int) -> SkeinM s r a ()
contract (Vertex !v, !p) = ask >>= \ !s -> lift $ do
	(v', p') <- neighbourST (v, p) s
	d <- vertexDegreeST v s
	if v' == v
		then case () of
			_ | p' == (p + 1) `mod` d -> contractLoopST (v, p) s
			  | p' == (p - 1) `mod` d -> contractLoopST (v, p') s
			  | otherwise             -> fail $ printf "contract: not loop (%i, %i) <-> (%i, %i) / %i" v p v' p' d
		else do
			d' <- vertexDegreeST v' s
			if d' < d
				then contractEdgeST (v', p') s
				else contractEdgeST (v, p) s


runSkein :: (SkeinRelation r a, SkeinKnotted k c d) => r -> (forall s. [Vertex] -> SkeinM s r a ()) -> k ArbitraryCrossing -> a
runSkein rel action knot = runST $ do
	(stateFromKnotted rel knot >>=) $ runReaderT $ do
		fix $ \ continue -> do
			num <- numberOfAliveVertices
			when (num > 0) $ do
				aliveVertices >>= action
				continue

		result <- ask >>= \ !s -> lift $ extractMultipleST s
		return $! finalNormalization rel knot result
