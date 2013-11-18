module Math.Topology.Manifolds.SurfaceGraph.SphereStar
    ( sphereStarDecomposition
    ) where

import Data.Ix (Ix)
import Data.Function (fix)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray, thaw)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray, STUArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, forM, filterM)
import Math.Topology.Manifolds.SurfaceGraph.Definition
import Math.Topology.Manifolds.SurfaceGraph.SphereStar.Backtrack


sphereStarDecomposition
    :: SurfaceGraph a ->
        ( Vertex SurfaceGraph b
        , Vertex SurfaceGraph c
        , Dart SurfaceGraph b -> Maybe (Dart SurfaceGraph c)
        , Dart SurfaceGraph c -> Dart SurfaceGraph b
        )

sphereStarDecomposition graph
    | eulerChar graph == 2  = error "sphereStarDecomposition: undefined for planar graphs"
    | otherwise             = runST $ do
        let (_, edgeTreeMarks) = backtrack graph

        edgeMarks <- (thaw :: (Ix i) => UArray i Bool -> ST s (STUArray s i Bool)) edgeTreeMarks
        let reductionSteps = 6 * numberOfEdges graph
        flip fix (nthDart graph 0, [], reductionSteps) $ \ loop (d, stack, depth) ->
            when (depth > 0) $ do
                mark <- readArray edgeMarks d
                if mark
                    then loop (nextCCW (opposite d), stack, depth - 1)
                    else case stack of
                        h : rest | h == opposite d -> do
                            writeArray edgeMarks d True
                            writeArray edgeMarks h True
                            loop (nextCCW d, rest, reductionSteps)
                        _                          -> loop (nextCCW d, d : stack, depth - 1)

        externalEdges <- filterM (fmap not . readArray edgeMarks) $ allHalfEdges graph
        let numberOfExternalEdges = length externalEdges

        borderPlace <- (newArray :: (Ix i) => (i, i) -> Int -> ST s (STUArray s i Int)) (dartsRange graph) (-1)
        borderConn <- (newArray_ :: (Ix i) => (i, i) -> ST s (STArray s i a)) (0, numberOfExternalEdges - 1)
        flip fix (0, head externalEdges) $ \ loop (!free, !edge) ->
                when (free < numberOfExternalEdges) $ do
                    mark <- readArray edgeMarks edge
                    if mark
                        then loop (free, nextCW $ opposite edge)
                        else do
                            writeArray borderPlace edge free
                            writeArray borderConn free edge
                            loop (free + 1, nextCW edge)

        sphere <- do
            sphereHead <- forM [0 .. numberOfExternalEdges - 1] $ \ !i -> do
                d <- readArray borderConn i
                return $!
                    let (v, p) = beginPair d
                    in (1 + vertexIndex v, p)

            sphereBody <- forM (allVertices graph) $ \ vertex ->
                forM (outcomingDarts vertex) $ \ d -> do
                    bp <- readArray borderPlace d
                    return $! if bp < 0
                        then let (v, p) = beginPair (opposite d)
                             in (1 + vertexIndex v, p)
                        else (0, bp)

            return $! constructFromList (sphereHead : sphereBody)

        star <- do
            starHead <- forM [0 .. numberOfExternalEdges - 1] $ \ !i -> do
                d <- readArray borderConn i
                bp <- readArray borderPlace (opposite d)
                return (0, bp)
            return $! constructFromList [starHead]

        let sphereRoot = nthVertex sphere 0
            starRoot = nthVertex star 0

            sphereToStarProjection d
                | beginVertex d == sphereRoot  = Just $! nthOutcomingDart starRoot (beginPlace d)
                | otherwise                    = Nothing

            starToSphereProjection d
                | beginVertex d == starRoot  = nthOutcomingDart sphereRoot (beginPlace d)
                | otherwise                  = error "starToSphereProjection: taken from dart that does not belong to star"

        return (sphereRoot, starRoot, sphereToStarProjection, starToSphereProjection)
