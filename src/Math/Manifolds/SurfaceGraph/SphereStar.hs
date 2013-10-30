module Math.Manifolds.SurfaceGraph.SphereStar
    ( sphereStarDecomposition
    ) where

import Data.Function (fix)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray, thaw)
import Data.Array.ST (STArray, STUArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, forM, filterM)
import Math.Manifolds.SurfaceGraph.Definition
import Math.Manifolds.SurfaceGraph.Util
import Math.Manifolds.SurfaceGraph.SphereStar.Backtrack


sphereStarDecomposition :: SurfaceGraph -> (SurfaceGraph, SurfaceGraph)
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

        return (sphere, star)
