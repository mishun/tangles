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
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.SkeinM.State
import Math.KnotTh.Invariants.Skein.SkeinM.ContractEdge
import Math.KnotTh.Invariants.Skein.SkeinM.Reduction


type SkeinM s r a = ReaderT (SkeinState s r a) (ST s)


newtype Vertex = Vertex Int deriving (Eq)


vertexDegree :: (SkeinRelation r a) => Vertex -> SkeinM s r a Int
vertexDegree (Vertex v) = ask >>= \ s -> lift $ vertexDegreeST s v


neighbour :: (SkeinRelation r a) => (Vertex, Int) -> SkeinM s r a (Vertex, Int)
neighbour (Vertex !v, p) = ask >>= \ !s -> lift $ do
    (!u, !q) <- neighbourST s (v, p)
    when (u == 0) $ fail $ printf "touching border at (%i, %i) <-> (%i, %i)" v p u q
    return $! (Vertex u, q)


contract :: (SkeinRelation r a) => (Vertex, Int) -> SkeinM s r a ()
contract (Vertex !v, !p) = ask >>= \ !s -> lift $ contractEdgeST s (v, p)


runSkein ::
    (SkeinRelation r a, SkeinStructure k c d)
        => r
        -> (forall s. [(Vertex, Int)] -> SkeinM s r a ())
        -> k ArbitraryCrossing
        -> ResultOnStructure k (SkeinRelationModel r) a

runSkein rel action knot =
    resultOnStructure rel knot $ runST $ do
        s <- stateFromKnotted rel knot
        fix $ \ continue -> do
            greedyReductionST s
            e <- internalEdgesST s
            case e of
                [] -> extractStateSumST s
                _  -> do
                    runReaderT (action $ map (\ (v, p) -> (Vertex v, p)) e) s
                    continue
