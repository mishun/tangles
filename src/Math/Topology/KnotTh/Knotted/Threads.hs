module Math.Topology.KnotTh.Knotted.Threads
    ( ThreadedCrossing(..)
    , allThreads
    , numberOfThreads
    , allThreadsWithMarks
    ) where

import Control.Monad (foldM)
import qualified Control.Monad.ST as ST
import Data.Maybe (fromJust)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Math.Topology.KnotTh.Knotted


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


allThreadsWithMarks :: (ThreadedCrossing a, Knotted k) => k a -> (Int, Dart k a -> Int, [(Int, [(Dart k a, Dart k a)])])
allThreadsWithMarks knot = ST.runST $ do
    let lp = numberOfFreeLoops knot
    threads <- newSTRef $ replicate lp (0, [])

    (n, visited) <- if numberOfEdges knot == 0
        then return (1, undefined)
        else do
            visited <- UMV.replicate (numberOfDarts knot) 0
            n <- foldM (\ !i (!startA, !startB) -> do
                    v <- UMV.read visited (dartIndex startA)
                    if v /= 0
                        then return $! i
                        else do
                            let traceBack !prev !b = do
                                    let a = opposite b
                                    UMV.write visited (dartIndex a) i
                                    UMV.write visited (dartIndex b) (-i)
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
                                            UMV.write visited (dartIndex a) i
                                            UMV.write visited (dartIndex b) (-i)
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
            (,) n `fmap` UV.unsafeFreeze visited

    (,,) (n - 1 + lp) ((visited UV.!) . dartIndex) `fmap` readSTRef threads
