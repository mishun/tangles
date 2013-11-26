{-# LANGUAGE RankNTypes #-}
module Math.Topology.KnotTh.EmbeddedLink.TangleStarGlue
    ( StarType(..)
    , tangleStarGlue
    , splitIntoTangleAndStar
    ) where

import qualified Data.Set as S
import Control.Arrow (first)
import Control.Monad.State.Strict (lift, evalStateT, gets, modify)
import Control.Monad (when, forM_)
import qualified Math.Algebra.Group.Dn as Dn
import Math.Combinatorics.ChordDiagram (generateNonPlanarRaw, generateBicolourableNonPlanarRaw, listChordDiagrams)
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.Construction (fromTangleAndStar)
import Math.Topology.Manifolds.SurfaceGraph


data StarType = BicolourableStar | AnyStar


tangleStarGlue
    :: (Monad m, CrossingType t) => StarType
        -> (forall m'. (Monad m') => ((Tangle (Crossing t), Dn.DnSubGroup) -> m' ()) -> m' ())
            -> (EmbeddedLink (Crossing t) -> m ()) -> m ()

tangleStarGlue starType tangleGenerator yield =
    let generator =
            case starType of
                BicolourableStar -> generateBicolourableNonPlanarRaw
                AnyStar          -> generateNonPlanarRaw
    in flip evalStateT S.empty $
        tangleGenerator $ \ (!tangle, !tangleSymmetry) ->
            let l = numberOfLegs tangle
            in forM_ (listChordDiagrams $ generator (l `div` 2)) $ \ (!star, (!starMirror, !starPeriod)) ->
                let variants = do
                        rot <- [0 .. gcd starPeriod (Dn.rotationPeriod tangleSymmetry) - 1]
                        mir <- if not starMirror && not (Dn.hasReflectionPart tangleSymmetry)
                                   then [False, True]
                                   else [False]
                        return $! Dn.fromReflectionRotation l (mir, rot)

                in forM_ variants $ \ !g -> do
                    let link = fromTangleAndStar star $ transformTangle g tangle
                        token = homeomorphismInvariant link
                    new <- gets (S.notMember token)
                    when new $ do
                        modify (S.insert token)
                        lift $ yield link


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
