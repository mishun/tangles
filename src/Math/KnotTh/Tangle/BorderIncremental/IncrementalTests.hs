module Math.KnotTh.Tangle.BorderIncremental.IncrementalTests
    ( testNoMultiEdges
    , testNo2ndReidemeisterReduction
    , testFlow4
    ) where

import Data.Array.Base (newArray, newArray_, unsafeRead, unsafeWrite)
import Data.Array.ST (STArray, STUArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef, modifySTRef')
import Control.Monad.ST (ST, runST)
import Control.Monad (when, unless, forM_)
import Math.KnotTh.Tangle
import Math.KnotTh.Tangle.NonAlternating


testNoMultiEdges :: Dart Tangle ct -> Int -> Bool
testNoMultiEdges leg gl =
    let ls = take gl $! iterate nextCW leg
    in and $! zipWith (\ !a !b ->
            let a' = opposite a
                b' = opposite b
            in isLeg a' || isLeg b' || incidentCrossing a' /= incidentCrossing b' 
        ) ls $! tail ls


testNo2ndReidemeisterReduction :: ArbitraryCrossingState -> Dart Tangle ArbitraryCrossing -> Int -> Bool
testNo2ndReidemeisterReduction cr leg gl =
    let legs = take gl $ iterate nextCW leg
        test (i, a, b)
            | isLeg a' || isLeg b' || incidentCrossing a' /= incidentCrossing b'                      = True
            | (passOver a' == passOverByDartId cr i) && (passOver b' == passOverByDartId cr (i + 1))  = False
            | otherwise                                                                               = True
            where
                a' = opposite a
                b' = opposite b
    in all test $ zip3 [0 ..] legs (tail legs)


testFlow4 :: Crossing Tangle ct -> Bool
testFlow4 finish = runST $ do
    let tangle = crossingTangle finish
    let n = numberOfCrossings tangle
    let l = numberOfLegs tangle

    flow <- newArray (0, 4 * n - 1) 0 :: ST s (STUArray s Int Int)
    total <- newSTRef =<<
        foldMIncidentDarts finish (\ !d !f ->
            if isLeg (opposite d)
                then unsafeWrite flow (dartIndex d) (-1) >> (return $! f + 1)
                else return $! f
        ) (0 :: Int)

    let push = do
            v <- newArray (0, n) False :: ST s (STUArray s Int Bool)
            p <- newArray_ (0, n)  :: ST s (STArray s Int (Dart Tangle ct))
            q <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
            tl <- newSTRef 0

            let touch !d = do
                    let ci = crossingIndex $! incidentCrossing d
                    visited <- unsafeRead v ci
                    unless visited $ do
                        unsafeWrite v ci True
                        unsafeWrite p ci d
                        i <- readSTRef tl
                        unsafeWrite q i ci
                        writeSTRef tl $! i + 1

            forM_ (allLegs tangle) $ \ !a -> do
                let b = opposite a
                when (isDart b) $ do
                    f <- unsafeRead flow $! dartIndex b
                    when (f > -1) (touch b)

            when (l == 4) $ do
                let d = opposite $! nthLeg tangle 2
                when (isDart d) (touch d)

            let loop !h = do
                    cont <- readSTRef tl >>= \ !t -> return $! (t > h)
                    when cont $ do
                        ci <- unsafeRead q h
                        forMIncidentDarts (nthCrossing tangle ci) $ \ !a -> do
                            let b = opposite a
                            when (isDart b) $ do
                                f <- unsafeRead flow $! dartIndex b
                                when (f > -1) (touch b)
                        loop $! h + 1

            loop 0

            pathFound <- unsafeRead v $! crossingIndex finish
            if pathFound
                then do
                    let update !a = do
                            unsafeRead flow (dartIndex a) >>= \ !f -> unsafeWrite flow (dartIndex a) $! f - 1
                            let b = opposite a
                            when (isDart b) $ do
                                unsafeRead flow (dartIndex b) >>= \ !f -> unsafeWrite flow (dartIndex b) $! f + 1
                                unsafeRead p (crossingIndex $! incidentCrossing b) >>= update

                    unsafeRead p (crossingIndex finish) >>= update
                    modifySTRef' total (+ 1)
                    push
                else do
                    final <- readSTRef total
                    foldMIncidentDarts finish (\ !a !ok -> do
                            let b = opposite a
                            if isDart b
                                then unsafeRead v (crossingIndex $! incidentCrossing b) >>= \ !ok' -> return $! ok' && ok
                                else return $! ok
                        ) (final == 4)

    push
