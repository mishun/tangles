module Math.Manifolds.SurfaceGraph.Embedding.QuadraticInitialization
    ( quadraticInitialization
    ) where

import qualified Data.Sequence as Seq
import System.IO.Unsafe (unsafePerformIO)
import Data.IORef (newIORef, readIORef, writeIORef, modifyIORef)
import Data.Array.IArray ((!), listArray)
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.IO (IOUArray)
import Control.Monad (when, unless, forM_, forM, liftM)
import Math.Manifolds.SurfaceGraph.Definition
import Math.Manifolds.SurfaceGraph.Util
import Math.Manifolds.SurfaceGraph.Embedding.Optimization


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
