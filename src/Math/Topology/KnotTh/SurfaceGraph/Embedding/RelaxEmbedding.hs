module Math.Topology.KnotTh.SurfaceGraph.Embedding.RelaxEmbedding
    ( relaxEmbedding
    ) where

import Control.Monad (unless, forM_, forM)
import Data.Array (Array)
import Data.Array.IArray ((!), listArray)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray, freeze)
import Data.Array.IO (IOArray, IOUArray)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Data.Ix (Ix)
import System.IO.Unsafe (unsafePerformIO)
import Math.Topology.KnotTh.SurfaceGraph.Definition
import Math.Topology.KnotTh.SurfaceGraph.Embedding.Optimization


relaxEmbedding :: Int
    -> Either (Vertex SurfaceGraph a) (Face SurfaceGraph a)
        -> Array (Dart SurfaceGraph a) [(Double, Double)]
            -> Array (Dart SurfaceGraph a) [(Double, Double)]

relaxEmbedding borderSegments root initial
    | not $ all (even . vertexDegree) $ allVertices g        = error "relaxEmbedding: all vertices must have even degree"
    | not $ all ((> 1) . length . (initial !)) $ allDarts g  = error "relaxEmbedding: there must be at least 2 point at every dart"
    | otherwise                                              = unsafePerformIO $ do
        let numberOfFrozenPoints =
                case root of
                    Left v  -> vertexDegree v
                    Right _ -> 0

        let totalNumberOfPoints =
                sum (map ((\ x -> x - 2) . length . (initial !) . fst) $ allEdges g)
                    + numberOfVertices g + max 0 (numberOfFrozenPoints - 1)

            numberOfMovablePoints = totalNumberOfPoints - numberOfFrozenPoints

        dartBeginIndex <- do
            dartBeginIndex <- (newArray_ :: (Ix i) => (i, i) -> IO (IOUArray i Int)) (dartsRange g)
            forM_ (allDarts g) $ \ !d ->
                writeArray dartBeginIndex d $
                    let v = beginVertex d
                    in case root of
                        Left start ->
                            case compare v start of
                                EQ -> totalNumberOfPoints - vertexDegree start + beginPlace d
                                LT -> vertexIndex v
                                GT -> vertexIndex v - 1
                        Right _    -> vertexIndex v

            (freeze :: (Ix i) => IOUArray i Int -> IO (Array i Int)) dartBeginIndex

        (dartIndices, threads) <- do
            dartIndices <- (newArray_ :: (Ix i) => (i, i) -> IO (IOArray i a)) (dartsRange g)
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

            visited <- (newArray :: (Ix i) => (i, i) -> Bool -> IO (IOUArray i Bool)) (dartsRange g) False

            let walk thread first a = do
                    let b = opposite a
                    writeArray visited a True
                    writeArray visited b True
                    nextThread <- ((++ thread) . reverse . tail) `fmap` allocate a
                    let v = beginVertex b
                    let cont = nthOutcomingDart v $ beginPlace b + (vertexDegree v `div` 2)
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
                Left v -> forM_ (outcomingDarts v) tryWalk
                _      -> return ()
            forM_ (allDarts g) tryWalk

            dartIndices' <- (freeze :: (Ix i) => IOArray i a -> IO (Array i a)) dartIndices
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

        forM_ (allEdges g) $ \ (d, _) ->
            forM_ (zip (initial ! d) (dartIndices ! d)) $ \ ((x, y), i) -> do
                writeArray coords (2 * i) (realToFrac x)
                writeArray coords (2 * i + 1) (realToFrac y)

        relaxEmbedding' interaction borderSegments numberOfMovablePoints numberOfFrozenPoints coords threads $
            let aliveVertices = filter ((/= root) . Left) $! allVertices g
            in map (map ((!! 1) . (dartIndices !)) . outcomingDarts) aliveVertices

        fmap (listArray (dartsRange g)) $ forM (allDarts g) $ \ d ->
            forM (dartIndices ! d) $ \ i -> do
                x <- readArray coords (2 * i)
                y <- readArray coords (2 * i + 1)
                return (realToFrac x, realToFrac y)

    where
        g = case root of
            Left v  -> vertexOwner v
            Right f -> faceOwner f
