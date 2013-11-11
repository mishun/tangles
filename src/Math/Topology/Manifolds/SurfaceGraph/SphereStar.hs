module Math.Topology.Manifolds.SurfaceGraph.SphereStar
    ( sphereStarDecomposition
    ) where

import Data.Function (fix)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray, thaw)
import Data.Array.ST (STArray, STUArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, forM, filterM)
import Math.Topology.Manifolds.SurfaceGraph.Definition
import Math.Topology.Manifolds.SurfaceGraph.Util
import Math.Topology.Manifolds.SurfaceGraph.SphereStar.Backtrack


sphereStarDecomposition :: SurfaceGraph -> (Vertex, Vertex, Dart -> Maybe Dart, Dart -> Dart)
sphereStarDecomposition graph
    | eulerChar graph == 2  = error "sphereStarDecomposition: undefined for planar graphs"
    | otherwise             = runST $ do
        let (_, edgeTreeMarks) = backtrack graph

        edgeMarks <- thaw edgeTreeMarks :: ST s (STUArray s Dart Bool)
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

        externalEdges <- filterM (fmap not . readArray edgeMarks) $ graphDarts graph
        let numberOfExternalEdges = length externalEdges

        borderPlace <- newArray (dartsRange graph) (-1) :: ST s (STUArray s Dart Int)
        borderConn <- newArray_ (0, numberOfExternalEdges - 1) :: ST s (STArray s Int Dart)
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
                    let (v, p) = begin d
                    in (1 + vertexIndex v, p)

            sphereBody <- forM (graphVertices graph) $ \ vertex ->
                forM (dartsIncidentToVertex vertex) $ \ d -> do
                    bp <- readArray borderPlace d
                    return $! if bp < 0
                        then let (v, p) = begin (opposite d)
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
                | beginVertex d == sphereRoot  = Just $! nthDartIncidentToVertex starRoot (beginPlace d)
                | otherwise                    = Nothing

            starToSphereProjection d
                | beginVertex d == starRoot  = nthDartIncidentToVertex sphereRoot (beginPlace d)
                | otherwise                  = error "starToSphereProjection: taken from dart that does not belong to star"

        return (sphereRoot, starRoot, sphereToStarProjection, starToSphereProjection)
