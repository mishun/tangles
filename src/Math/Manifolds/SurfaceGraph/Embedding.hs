module Math.Manifolds.SurfaceGraph.Embedding
    ( embeddingInCircleWithVertexRooting
    , embeddingInCircleWithFaceRooting
    , embeddingInPolygonWithGrouping
    ) where

import qualified Data.Sequence as Seq
import System.IO.Unsafe (unsafePerformIO)
import Data.Ord (comparing)
import Data.Maybe (fromJust)
import Data.List (find, groupBy, sortBy)
import Data.Array.Base ((!), array, listArray, newArray, newArray_, freeze, readArray, writeArray)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.IO (IOArray, IOUArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Control.Monad (forM, forM_, when, unless, liftM)
import Math.Manifolds.SurfaceGraph.Definition
import Math.Manifolds.SurfaceGraph.Util
import Math.Manifolds.SurfaceGraph.Barycentric
import Math.Manifolds.SurfaceGraph.Embedding.Optimization


embeddingInCircleWithVertexRooting :: Int -> Vertex -> Array Dart [(Double, Double)]
embeddingInCircleWithVertexRooting smoothing vertex =
    circleEmbedding (max 1 smoothing) (Left vertex)


embeddingInCircleWithFaceRooting :: Int -> Face -> Array Dart [(Double, Double)]
embeddingInCircleWithFaceRooting smoothing face =
    circleEmbedding (max 1 smoothing) (Right face)


embeddingInPolygonWithGrouping :: (Dart -> Dart -> Bool) -> Int -> Vertex -> (Int, Array Dart [(Double, Double)])
embeddingInPolygonWithGrouping sameGroup smoothing root
    | numberOfGroups < 3  = error "embeddingInPolygonWithGrouping: there are less than 3 groups"
    | otherwise           = (numberOfGroups, relaxEmbedding (Left root) embedding)
    where
        graph = vertexOwnerGraph root

        groups = let ds = [0 :: Int ..] `zip` dartsIncidentToVertex root
                 in groupBy (\ (_, a) (_, b) -> sameGroup a b) $
                     take (length ds) $
                         dropWhile (\ (_, d) -> sameGroup (nextCW d) d) $
                             ds ++ ds

        numberOfGroups = length groups

        borderInit = map snd $ sortBy (comparing fst) $ do
            let halfAngle = pi / fromIntegral numberOfGroups
                x0 = cos halfAngle
                h = sin halfAngle

            (groupId, group) <- [0 :: Int .. ] `zip` groups
            let groupSize = length group
                a = 2 * halfAngle * fromIntegral groupId

            (inGroupId, (index, _)) <- [0 :: Int ..] `zip` group
            let y0 = 2 * h * (-0.5 + fromIntegral (inGroupId + 1) / fromIntegral (groupSize + 1))
                x = cos a * x0 - sin a * y0
                y = sin a * x0 + cos a * y0
            return (index, (x, y))

        initial = quadraticInitialization 0.99 root borderInit

        embedding = listArray (dartsRange graph) $ map (\ d -> [initial ! d, initial ! opposite d]) $ graphDarts graph


circleEmbedding :: Int -> Either Vertex Face -> Array Dart [(Double, Double)]
circleEmbedding subdivisionOrder root =
    relaxEmbedding root $ barycentricImproving (\ (Left v) -> quadraticEmbedding v) subdivisionOrder root


barycentricImproving :: (Either Vertex Face -> Array Dart [(Double, Double)]) -> Int -> Either Vertex Face -> Array Dart [(Double, Double)]
barycentricImproving f order root
    | order <= 0  = f root
    | otherwise   =
        let g = case root of { Left vertex -> vertexOwnerGraph vertex ; Right face -> faceOwnerGraph face }
            (_, vv, vf, vd) = barycentricSubdivision' g
            be = barycentricImproving f (order - 1) $ Left $
                case root of
                    Left vertex -> fst $ fromJust $ find ((== vertex) . snd) vv
                    Right face  -> fst $ fromJust $ find ((== face) . snd) vf

        in array (dartsRange g) $ do
            (v, (a, b)) <- vd
            let l = reverse (be ! nthDartIncidentToVertex v 0) ++ tail (be ! nthDartIncidentToVertex v 2)
            [(a, l), (b, reverse l)]


quadraticEmbedding :: Vertex -> Array Dart [(Double, Double)]
quadraticEmbedding root =
    let g = vertexOwnerGraph root
        c = quadraticInitialization 0.99 root $!
            let k = vertexDegree root
            in map (\ !i -> let a = 2 * pi * fromIntegral i / fromIntegral k in (cos a, -sin a)) [0 .. k - 1]
    in listArray (dartsRange g) $! map (\ d -> [c ! d, c ! opposite d]) $! graphDarts g


quadraticInitialization :: Double -> Vertex -> [(Double, Double)] -> Array Dart (Double, Double)
quadraticInitialization seed s brd
    | vertexDegree s /= length brd  = error "quadraticInitialization: wrong number of elements in border"
    | otherwise                     = unsafePerformIO $ do
        let g = vertexOwnerGraph s
        let vi :: UArray Vertex Int
            vi = listArray (verticesRange g) $! scanl (\ !x !v -> if v == s then x else x + 1) 0 $! graphVertices g

        let n = numberOfVertices g - 1
        x <- newArray (0, n - 1) 0
        y <- newArray (0, n - 1) 0
        do
            dist <- do
                d <- newArray (verticesRange g) (-1) :: IO (IOUArray Vertex Int)
                writeArray d s 0
                q <- newIORef $ Seq.singleton s

                let isEmpty = liftM Seq.null $ readIORef q
                let enqueue u = modifyIORef q (Seq.|> u)
                let dequeue = do
                        qc <- readIORef q
                        let (u Seq.:< rest) = Seq.viewl qc
                        writeIORef q rest
                        return $! u

                let loop = do
                        empty <- isEmpty
                        unless empty $ do
                            u <- dequeue
                            du <- readArray d u
                            forM_ (adjacentVertices u) $ \ v -> do
                                dv <- readArray d v
                                when (dv < 0) $ do
                                    writeArray d v (du + 1)
                                    enqueue v
                            loop

                loop
                unsafeFreeze d :: IO (UArray Vertex Int)

            let dartWeight d =
                    let l = dist ! beginVertex d
                        r = dist ! endVertex d
                    in realToFrac seed ** realToFrac (min l r)

            let defs =
                    let a = flip map (filter (/= s) $ graphVertices g) $ \ !v ->
                                let !i = vi ! v
                                    !w = sum $ map dartWeight $ dartsIncidentToVertex v
                                in (i, i, w)
                        b = flip map (filter (\ !d -> beginVertex d /= s && endVertex d /= s) $ graphDarts g) $ \ !d ->
                                let !i = vi ! beginVertex d
                                    !j = vi ! endVertex d
                                    !w = -(dartWeight d)
                                in (i, j, w)
                    in a ++ b

            let rp = flip map (filter ((/= s) . endVertex . snd) $ zip brd $ dartsIncidentToVertex s) $ \ ((!cx, !cy), !d) ->
                        let !i = vi ! endVertex d
                            !w = dartWeight d
                        in (i, cx * w, cy * w)

            conjugateGradientSolve' n defs rp (x, y)

        let border :: Array Int (Double, Double)
            border = listArray (0, length brd - 1) brd
        result <- forM (graphDarts g) $ \ !d -> do
                let (v, p) = begin d
                if v == s
                    then return $! border ! p
                    else do
                        cx <- readArray x (vi ! v)
                        cy <- readArray y (vi ! v)
                        return (realToFrac cx, realToFrac cy)

        return $! listArray (dartsRange g) result


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
