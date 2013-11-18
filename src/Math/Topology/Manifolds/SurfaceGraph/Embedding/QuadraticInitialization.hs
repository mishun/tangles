module Math.Topology.Manifolds.SurfaceGraph.Embedding.QuadraticInitialization
    ( quadraticInitialization
    ) where

import Data.Ix (Ix)
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
import Math.Topology.Manifolds.SurfaceGraph.Definition
import Math.Topology.Manifolds.SurfaceGraph.Embedding.Optimization


quadraticInitialization :: Double -> Vertex SurfaceGraph a -> [(Double, Double)] -> Array (Dart SurfaceGraph a) (Double, Double)
quadraticInitialization seed s brd
    | vertexDegree s /= length brd  = error "quadraticInitialization: wrong number of elements in border"
    | otherwise                     = unsafePerformIO $ do
        let g = vertexOwner s
        let vi = (listArray :: (Ix i) => (i, i) -> [Int] -> UArray i Int) (verticesRange g) $
                    scanl (\ !x !v -> if v == s then x else x + 1) 0 $ allVertices g

        let n = numberOfVertices g - 1
        x <- newArray (0, n - 1) 0
        y <- newArray (0, n - 1) 0
        do
            dist <- do
                d <- (newArray :: (Ix i) => (i, i) -> Int -> IO (IOUArray i Int)) (verticesRange g) (-1)
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
                            forM_ (outcomingDarts u) $ \ uv -> do
                                let v = endVertex uv
                                dv <- readArray d v
                                when (dv < 0) $ do
                                    writeArray d v (du + 1)
                                    enqueue v
                            loop

                loop
                (unsafeFreeze :: (Ix i) => IOUArray i Int -> IO (UArray i Int)) d

            let dartWeight d =
                    let l = dist ! beginVertex d
                        r = dist ! endVertex d
                    in realToFrac seed ** realToFrac (min l r)

            let defs =
                    let a = flip map (filter (/= s) $ allVertices g) $ \ !v ->
                                let !i = vi ! v
                                    !w = sum $ map dartWeight $ outcomingDarts v
                                in (i, i, w)
                        b = flip map (filter (\ !d -> beginVertex d /= s && endVertex d /= s) $ allHalfEdges g) $ \ !d ->
                                let !i = vi ! beginVertex d
                                    !j = vi ! endVertex d
                                    !w = -(dartWeight d)
                                in (i, j, w)
                    in a ++ b

            let rp = flip map (filter ((/= s) . endVertex . snd) $ zip brd $ outcomingDarts s) $ \ ((!cx, !cy), !d) ->
                        let !i = vi ! endVertex d
                            !w = dartWeight d
                        in (i, cx * w, cy * w)

            conjugateGradientSolve' n defs rp (x, y)

        let border :: Array Int (Double, Double)
            border = listArray (0, length brd - 1) brd
        result <- forM (allHalfEdges g) $ \ !d -> do
                let (v, p) = beginPair d
                if v == s
                    then return $! border ! p
                    else do
                        cx <- readArray x (vi ! v)
                        cy <- readArray y (vi ! v)
                        return (realToFrac cx, realToFrac cy)

        return $! listArray (dartsRange g) result
