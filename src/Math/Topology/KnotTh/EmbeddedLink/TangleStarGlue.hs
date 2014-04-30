{-# LANGUAGE RankNTypes #-}
module Math.Topology.KnotTh.EmbeddedLink.TangleStarGlue
    ( tangleStarGlue
    , splitIntoTangleAndStar
    ) where

import qualified Data.Map as M
import Control.Arrow (first)
import Control.Monad.State.Strict (execState, modify)
import Control.DeepSeq
import Control.Parallel.Strategies (parMap, rdeepseq)
import qualified Math.Algebra.Group.Dn as Dn
import Math.Combinatorics.ChordDiagram (ChordDiagram)
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.Construction (fromTangleAndStar)
import Math.Topology.Manifolds.SurfaceGraph



tangleStarGlue
    :: (NFData a, Crossing a) => (Int -> [(ChordDiagram, (Bool, Int))])
        -> (forall m'. (Monad m') => ((Tangle a, (Dn.DnSubGroup, x)) -> m' ()) -> m' ())
            -> [EmbeddedLink a]

tangleStarGlue starGenerator tangleGenerator =
    let diagrams = map starGenerator [0 ..]

        tangles = execState (tangleGenerator $ \ (!tangle, (!symmetry, _)) -> modify ((tangle, symmetry) :)) []

    in M.elems $ M.unions $ parMap rdeepseq
        (\ (tangle, tangleSymmetry) ->
            M.fromList $ do
                let l = numberOfLegs tangle
                (star, (starMirror, starPeriod)) <- diagrams !! (l `div` 2)
                rot <- [0 .. gcd starPeriod (Dn.rotationPeriod tangleSymmetry) - 1]
                mir <- if not starMirror && not (Dn.hasReflectionPart tangleSymmetry)
                           then [False, True]
                           else [False]
                let g = Dn.fromReflectionRotation l (mir, rot)
                    link = fromTangleAndStar star $ transformTangle g tangle
                return (unrootedHomeomorphismInvariant link, link)
        ) tangles


splitIntoTangleAndStar :: EmbeddedLink a -> (Tangle a, Vertex SurfaceGraph a')
splitIntoTangleAndStar link =
    let g = constructFromList $
            map (map (first (+ (-1)) . endPair') . outcomingDarts) $
                allVertices link

        (sp, star, _, _) = sphereStarDecomposition g

        tangle =
            let border = map endPair' $ outcomingDarts sp
                body = do
                    v <- tail $ allVertices $ vertexOwner sp
                    return (map endPair' $ outcomingDarts v, vertexCrossing $ nthVertex link $ vertexIndex v)
            in implode (numberOfFreeLoops link, border, body)

    in (tangle, star)
