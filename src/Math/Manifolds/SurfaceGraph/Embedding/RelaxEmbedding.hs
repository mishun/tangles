module Math.Manifolds.SurfaceGraph.Embedding.RelaxEmbedding
    ( relaxEmbedding
    ) where

import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Data.Array.IArray ((!), listArray)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray, freeze)
import Data.Array (Array)
import Data.Array.IO (IOArray, IOUArray)
import Control.Monad (unless, forM_, forM)
import Math.Manifolds.SurfaceGraph.Definition
import Math.Manifolds.SurfaceGraph.Util
import Math.Manifolds.SurfaceGraph.Embedding.Optimization


relaxEmbedding :: Either Vertex Face -> Array Dart [(Double, Double)] -> Array Dart [(Double, Double)]
relaxEmbedding root initial
    | not $ all (even . vertexDegree) $ graphVertices g        = error "relaxEmbedding: all vertices must have even degree"
    | not $ all ((> 1) . length . (initial !)) $ graphDarts g  = error "relaxEmbedding: there must be at least 2 point at every dart"
    | otherwise                                                = unsafePerformIO $ do
        let numberOfFrozenPoints = case root of { Left v -> vertexDegree v ; Right _ -> 0 }
        let totalNumberOfPoints =
                sum (map ((\ x -> x - 2) . length . (initial !) . fst) $ graphEdges g)
                    + numberOfVertices g + max 0 (numberOfFrozenPoints - 1)
        let numberOfMovablePoints = totalNumberOfPoints - numberOfFrozenPoints

        dartBeginIndex <- do
            dartBeginIndex <- newArray_ (dartsRange g) :: IO (IOUArray Dart Int)
            forM_ (graphDarts g) $ \ !d ->
                writeArray dartBeginIndex d $
                    let v = beginVertex d
                    in case root of
                        Left start ->
                            case compare v start of
                                EQ -> totalNumberOfPoints - vertexDegree start + snd (begin d)
                                LT -> vertexIndex v
                                GT -> vertexIndex v - 1
                        Right _    -> vertexIndex v

            freeze dartBeginIndex :: IO (Array Dart Int)

        (dartIndices, threads) <- do
            dartIndices <- newArray_ (dartsRange g) :: IO (IOArray Dart [Int])
            freeIndex <- newIORef (case root of { Left _ -> numberOfVertices g - 1 ; Right _ -> numberOfVertices g })

            let allocate a = do
                    let b = opposite a
                    let len = length (initial ! a) - 2
                    gotIndex <- readIORef freeIndex
                    writeIORef freeIndex (gotIndex + len)
                    let list = [dartBeginIndex ! a] ++ [gotIndex .. gotIndex + len - 1] ++ [dartBeginIndex ! b]
                    writeArray dartIndices a list
                    writeArray dartIndices b $! reverse list
                    return $! list

            visited <- newArray (dartsRange g) False :: IO (IOUArray Dart Bool)

            let walk thread first a = do
                    let b = opposite a
                    writeArray visited a True
                    writeArray visited b True
                    nextThread <- ((++ thread) . reverse . tail) `fmap` allocate a
                    let v = beginVertex b
                    let cont = nthDartIncidentToVertex v $ snd (begin b) + (vertexDegree v `div` 2)
                    if Left v == root || cont == first
                        then return $! nextThread
                        else walk nextThread first cont

            threads <- newIORef []
            let tryWalk d = do
                    v <- readArray visited d
                    unless v $ do
                        thread <- walk [dartBeginIndex ! d] d d
                        modifyIORef threads (thread :)

            case root of
                Left v -> forM_ (dartsIncidentToVertex v) tryWalk
                _      -> return ()
            forM_ (graphDarts g) tryWalk

            dartIndices' <- freeze dartIndices :: IO (Array Dart [Int])
            threads' <- readIORef threads
            return (dartIndices', threads')

        let interaction = InteractionConst
                { interactionBorder   = 2
                , interactionElectric = 0.5
                , interactionBend     = 15
                , interactionElastic  = 10
                , interactionCross    = 1.5
                }

        coords <- newArray (0, 2 * totalNumberOfPoints - 1) 0.0

        forM_ (graphEdges g) $ \ (d, _) ->
            forM_ (zip (initial ! d) (dartIndices ! d)) $ \ ((x, y), i) -> do
                writeArray coords (2 * i) (realToFrac x)
                writeArray coords (2 * i + 1) (realToFrac y)

        relaxEmbedding' interaction False numberOfMovablePoints numberOfFrozenPoints coords threads $
            let aliveVertices = filter ((/= root) . Left) $! graphVertices g
            in map (map ((!! 1) . (dartIndices !)) . dartsIncidentToVertex) aliveVertices

        fmap (listArray (dartsRange g)) $ forM (graphDarts g) $ \ d -> forM (dartIndices ! d) $ \ i -> do
            x <- readArray coords (2 * i)
            y <- readArray coords (2 * i + 1)
            return (realToFrac x, realToFrac y)

    where
        g = case root of
            Left v  -> vertexOwnerGraph v
            Right f -> faceOwnerGraph f
