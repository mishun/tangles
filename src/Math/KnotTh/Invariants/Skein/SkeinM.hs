{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Invariants.Skein.SkeinM
	( module Math.KnotTh.Invariants.Skein.Relation
	, SkeinM
	, Vertex
	, vertexDegree
	, neighbour
	, contract
	, runSkein
	) where

import Data.Function (fix)
import Control.Monad.ST (ST, runST)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad (when)
import Text.Printf
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.SkeinM.Def
import Math.KnotTh.Invariants.Skein.SkeinM.Basic
import Math.KnotTh.Invariants.Skein.SkeinM.ContractEdge
import Math.KnotTh.Invariants.Skein.SkeinM.Reduction
import Debug.Trace


type SkeinM s r a = ReaderT (SkeinState s r a) (ST s)


newtype Vertex = Vertex Int deriving (Eq)


vertexDegree :: Vertex -> SkeinM s r a Int
vertexDegree (Vertex v) = ask >>= \ s -> lift $ vertexDegreeST s v


neighbour :: (Vertex, Int) -> SkeinM s r a (Vertex, Int)
neighbour (Vertex !v, p) = ask >>= \ !s -> lift $ do
	(!u, !q) <- neighbourST s (v, p)
	when (u == 0) $ fail $ printf "touching border at (%i, %i) <-> (%i, %i)" v p u q
	return $! (Vertex u, q)


contract :: (SkeinRelation r a) => (Vertex, Int) -> SkeinM s r a ()
contract (Vertex !v, !p) = ask >>= \ !s -> lift $ contractEdgeST s (v, p)


runSkein :: (SkeinRelation r a, SkeinKnotted k c d) => r -> (forall s. [Vertex] -> SkeinM s r a ()) -> k ArbitraryCrossing -> a
runSkein rel action knot = runST $ do
	(stateFromKnotted rel knot >>=) $ runReaderT $ do
		fix $ \ continue -> do
			aliveVertices <- ask >>= \ s -> lift $ do
				dumpStateST s >>= flip trace (return ())
				greedyReductionST s
				dumpStateST s >>= flip trace (return ())
				aliveVerticesST s

			case aliveVertices of
				[] -> return ()
				_  -> do
					action $ map Vertex aliveVertices
					continue

		result <- ask >>= \ !s -> lift $ extractMultipleST s
		return $! finalNormalization rel knot (result * (circleFactor rel ^ numberOfFreeLoops knot))
