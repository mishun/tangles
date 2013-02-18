module Math.KnotTh.Knotted.Threads
    ( ThreadedCrossing(..)
    , maybeThreadContinuation
    , allThreads
    , allThreadsWithMarks
    ) where

import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Array.Unboxed (UArray)
import Data.Array.Unsafe (unsafeFreeze)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Control.Monad.ST
import Control.Monad (foldM)
import Math.KnotTh.Knotted.KnottedDefinition.Knotted
import Math.KnotTh.Knotted.KnottedDefinition.Misc


class (CrossingType ct) => ThreadedCrossing ct where
    threadContinuation :: (Knotted k c d) => d ct -> d ct

    threadContinuation d
        | isDart d   = nextCCW $ nextCCW d
        | otherwise  = error "continuation: from endpoint"


{-# INLINE maybeThreadContinuation #-}
maybeThreadContinuation :: (ThreadedCrossing ct, Knotted k c d) => d ct -> Maybe (d ct)
maybeThreadContinuation d
    | isDart d   = Just $! threadContinuation d
    | otherwise  = Nothing


allThreads :: (ThreadedCrossing ct, Knotted k c d, Eq (d ct)) => k ct -> [[(d ct, d ct)]]
allThreads knot =
    let (_, _, threads) = allThreadsWithMarks knot
    in map snd threads


allThreadsWithMarks :: (ThreadedCrossing ct, Knotted k c d, Eq (d ct)) => k ct -> (Int, UArray Int Int, [(Int, [(d ct, d ct)])])
allThreadsWithMarks knot = runST $ do
    visited <- newArray (dartIndexRange knot) 0 :: ST s (STUArray s Int Int)
    threads <- newSTRef $ replicate (numberOfFreeLoops knot) (0, [])

    n <- flip (`foldM` 1) (allEdges knot) $ \ !i (!startA, !startB) -> do
        v <- readArray visited $ dartIndex startA
        if v /= 0
            then return $! i
            else do
                let traceBack !prev !b = do
                        let a = opposite b
                        writeArray visited (dartIndex a) i
                        writeArray visited (dartIndex b) (-i)
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
                            writeArray visited (dartIndex a) i
                            writeArray visited (dartIndex b) (-i)
                            traceFront ((a, b) : prev) b

                tb <- traceBack [] startB
                thread <- case tb of
                    Left thread  -> return $! thread
                    Right prefix -> do
                        !suffix <- traceFront [] startB
                        return $! prefix ++ suffix

                readSTRef threads >>= \ !list -> writeSTRef threads $! (i, thread) : list
                return $! i + 1

    visited' <- unsafeFreeze visited
    (,,) (n - 1 + numberOfFreeLoops knot) visited' `fmap` readSTRef threads
