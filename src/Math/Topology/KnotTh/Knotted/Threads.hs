module Math.Topology.KnotTh.Knotted.Threads
    ( ThreadList
    , ThreadedCrossing(..)
    , maybeThreadContinuation
    , allThreads
    , numberOfThreads
    , allThreadsWithMarks
    ) where

import Data.Ix (Ix)
import Data.STRef (newSTRef, readSTRef, modifySTRef')
import Data.Array.Unboxed (UArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Control.Monad.ST
import Control.Monad (foldM)
import Math.Topology.KnotTh.Knotted.Definition.Knotted
import Math.Topology.KnotTh.Knotted.Definition.Misc


type ThreadList dart = (Int, UArray dart Int, [(Int, [(dart, dart)])])


class (CrossingType ct) => ThreadedCrossing ct where
    threadContinuation :: (Knotted k) => Dart k ct -> Dart k ct

    threadContinuation d | isDart d   = nextCCW $ nextCCW d
                         | otherwise  = error "continuation: from endpoint"


{-# INLINE maybeThreadContinuation #-}
maybeThreadContinuation :: (ThreadedCrossing ct, Knotted k) => Dart k ct -> Maybe (Dart k ct)
maybeThreadContinuation d | isDart d   = Just $! threadContinuation d
                          | otherwise  = Nothing


allThreads :: (ThreadedCrossing ct, Knotted k, Ix (Dart k ct)) => k ct -> [[(Dart k ct, Dart k ct)]]
allThreads knot =
    let (_, _, threads) = allThreadsWithMarks knot
    in map snd threads


numberOfThreads :: (ThreadedCrossing ct, Knotted k, Ix (Dart k ct)) => k ct -> Int
numberOfThreads knot =
    let (n, _, _) = allThreadsWithMarks knot
    in n


allThreadsWithMarks :: (ThreadedCrossing ct, Knotted k, Ix (Dart k ct)) => k ct -> ThreadList (Dart k ct)
allThreadsWithMarks knot = runST $ do
    visited <- (newArray :: Ix i => (i, i) -> Int -> ST s (STUArray s i Int)) (dartsRange knot) 0
    threads <- newSTRef $ replicate (numberOfFreeLoops knot) (0, [])

    n <- flip (`foldM` 1) (allEdges knot) $ \ !i (!startA, !startB) -> do
        v <- readArray visited startA
        if v /= 0
            then return $! i
            else do
                let traceBack !prev !b = do
                        let a = opposite b
                        writeArray visited a i
                        writeArray visited b (-i)
                        let !next = (a, b) : prev
                        if isEndpoint a
                            then return $! Right $! next
                            else do
                                let b' = threadContinuation a
                                if b' == startB
                                    then return $! Left $! next
                                    else traceBack next b'

                let traceFront !prev !b'
                        | isEndpoint b'  = return $! reverse prev
                        | otherwise      = do
                            let !a = threadContinuation b'
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

    visited' <- unsafeFreeze visited
    (,,) (n - 1 + numberOfFreeLoops knot) visited' `fmap` readSTRef threads
