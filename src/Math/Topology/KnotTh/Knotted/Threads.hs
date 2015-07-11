module Math.Topology.KnotTh.Knotted.Threads
    ( ThreadList
    , ThreadedCrossing(..)
    , allThreads
    , numberOfThreads
    , allThreadsWithMarks
    ) where

import Control.Monad (foldM)
import qualified Control.Monad.ST as ST
import Data.Array.Unboxed (UArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Data.Ix (Ix)
import Data.Maybe (fromJust)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import Math.Topology.KnotTh.Knotted


type ThreadList d = (Int, UArray d Int, [(Int, [(d, d)])])


class ThreadedCrossing a where
    maybeThreadContinuation :: (Knotted k) => Dart k a -> Maybe (Dart k a)
    maybeThreadContinuation d = maybeBeginVertex d >> return (nextCCW $ nextCCW d)

    threadContinuation :: (Knotted k) => Dart k a -> Dart k a
    threadContinuation = fromJust . maybeThreadContinuation


allThreads :: (ThreadedCrossing a, Knotted k) => k a -> [[(Dart k a, Dart k a)]]
allThreads knot =
    let (_, _, threads) = allThreadsWithMarks knot
    in map snd threads


numberOfThreads :: (ThreadedCrossing a, Knotted k) => k a -> Int
numberOfThreads knot =
    let (n, _, _) = allThreadsWithMarks knot
    in n


allThreadsWithMarks :: (ThreadedCrossing a, Knotted k) => k a -> ThreadList (Dart k a)
allThreadsWithMarks knot = ST.runST $ do
    let lp = numberOfFreeLoops knot
    threads <- newSTRef $ replicate lp (0, [])

    (n, visited) <- if numberOfEdges knot == 0
        then return (1, undefined)
        else do
            visited <- (newArray :: (Ix i) => (i, i) -> Int -> ST.ST s (STUArray s i Int)) (dartsRange knot) 0
            n <- foldM (\ !i (!startA, !startB) -> do
                    v <- readArray visited startA
                    if v /= 0
                        then return $! i
                        else do
                            let traceBack !prev !b = do
                                    let a = opposite b
                                    writeArray visited a i
                                    writeArray visited b (-i)
                                    let !next = (a, b) : prev
                                    case maybeThreadContinuation a of
                                        Nothing                -> return $! Right $! next
                                        Just b' | b' == startB -> return $! Left $! next
                                                | otherwise    -> traceBack next b'

                                traceFront !prev !b' =
                                    case maybeThreadContinuation b' of
                                        Nothing -> return $! reverse prev
                                        Just !a -> do
                                            let !b = opposite a
                                            writeArray visited a i
                                            writeArray visited b (-i)
                                            traceFront ((a, b) : prev) b

                            tb <- traceBack [] startB
                            thread <- case tb of
                                Left thread  -> return $! thread
                                Right prefix -> do
                                    !suffix <- traceFront [] startB
                                    return $! prefix ++ suffix

                            modifySTRef' threads ((i, thread) :)
                            return $! i + 1
                ) 1 (allEdges knot)
            (,) n `fmap` unsafeFreeze visited

    (,,) (n - 1 + lp) visited `fmap` readSTRef threads
