{-# LANGUAGE Rank2Types #-}
module Math.KnotTh.Invariants.Skein.SkeinM
    ( module Math.KnotTh.Invariants.Skein.Relation
    , SkeinM
    , Vertex
    , StrategyResult(..)
    , vertexDegree
    , neighbour
    , runSkeinStrategy
    ) where

import Data.Function (fix)
import Control.Monad.ST (ST, runST)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad (when)
import Control.Arrow (first)
import Text.Printf
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.SkeinM.State
import Math.KnotTh.Invariants.Skein.SkeinM.ContractEdge
import Math.KnotTh.Invariants.Skein.SkeinM.Reduction


type SkeinM s r a = ReaderT (SkeinState s r a) (ST s)

newtype Vertex = Vertex Int deriving (Eq, Show)

data StrategyResult = Contract (Vertex, Int)


vertexDegree :: (SkeinRelation r a) => Vertex -> SkeinM s r a Int
vertexDegree (Vertex v) = ask >>= \ s -> lift $ vertexDegreeST s v


neighbour :: (SkeinRelation r a) => (Vertex, Int) -> SkeinM s r a (Vertex, Int)
neighbour (Vertex !v, p) = ask >>= \ !s -> lift $ do
    (!u, !q) <- neighbourST s (v, p)
    when (u == 0) $ fail $ printf "touching border at (%i, %i) <-> (%i, %i)" v p u q
    return (Vertex u, q)


runSkeinStrategy ::
    (SkeinRelation r a, SkeinStructure k c d)
        => r
        -> (forall s. [(Vertex, Int)] -> SkeinM s r a StrategyResult)
        -> k ArbitraryCrossing
        -> ResultOnStructure k (SkeinRelationModel r) a

runSkeinStrategy rel strategy knot =
    resultOnStructure rel knot $ runST $ do
        s <- stateFromKnotted rel knot
        fix $ \ continue -> do
            greedyReductionST s
            edges <- internalEdgesST s
            case edges of
                [] -> extractStateSumST s
                _  -> do
                    action <- runReaderT (strategy $ map (first Vertex) edges) s
                    case action of
                        Contract (Vertex v, p) -> contractEdgeST s (v, p)
                    continue
