{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Tangle.Generation.BorderIncremental.RootingTest
    ( rootingSymmetryTest
    , rootCodeLeg
    ) where

import Data.Function (fix)
import Data.Ord (comparing)
import Data.List (minimumBy)
import Data.Bits (shiftL)
import Data.Array.Base (unsafeRead, unsafeWrite, unsafeAt)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray, STUArray, runSTUArray, newArray, newArray_)
import Data.Array.Unsafe (unsafeFreeze)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, guard)
import qualified Math.Algebra.RotationDirection as R
import qualified Math.Algebra.Group.Dn as Dn
import qualified Math.Algebra.Group.D4 as D4
import Math.Topology.KnotTh.Tangle


rootingSymmetryTest :: (CrossingType ct) => Vertex Tangle ct -> Maybe (Dn.DnSubGroup, (D4.D4, D4.D4), UArray Int Int)
rootingSymmetryTest lastCrossing = do
    let tangle = vertexOwner lastCrossing
    guard $ numberOfLegs tangle >= 4
    cp <- investigateConnectivity lastCrossing
    analyseSymmetry lastCrossing (unsafeAt cp . vertexIndex)


rootCodeLeg :: (CrossingType ct) => Dart Tangle ct -> R.RotationDirection -> (D4.D4, UArray Int Int)
rootCodeLeg root dir
    | isDart root                     = error "rootCodeLeg: leg expected"
    | numberOfFreeLoops tangle /= 0   = error "rootCodeLeg: free loops present"
    | numberOfVertices tangle > 127   = error "rootCodeLeg: too many crossings"
    | otherwise                       = rootCodeLegUnsafe root dir
    where
        tangle = dartOwner root


investigateConnectivity :: Vertex Tangle ct -> Maybe (UArray Int Bool)
investigateConnectivity lastCrossing = runST $ do
    let tangle = vertexOwner lastCrossing
    let n = numberOfVertices tangle

    tins <- newArray (0, n) (-1) :: ST s (STUArray s Int Int)
    cp <- newArray (0, n) False :: ST s (STUArray s Int Bool)
    timer <- newSTRef 1

    let {-# INLINE dfs #-}
        dfs !v !from = do
            tin <- readSTRef timer
            writeSTRef timer $! tin + 1
            unsafeWrite tins (vertexIndex v) tin

            let {-# INLINE walk #-}
                walk !d (!fup, !border)
                    | isLeg d    = return (fup, border + 1)
                    | u == from  = return (fup, border)
                    | otherwise  = do
                        utin <- unsafeRead tins (vertexIndex u)
                        if utin > 0
                            then return (min fup utin, border)
                            else do
                                (!thatFup, !thatBorder) <- dfs u v
                                when (thatFup >= tin) (unsafeWrite cp (vertexIndex v) True)
                                return (min fup thatFup, border + if thatFup <= tin then thatBorder else 1)
                    where
                        u = beginVertex d

            foldMAdjacentDarts v walk (tin, 0 :: Int)

    (!_, !borderCut) <- dfs lastCrossing lastCrossing
    if borderCut <= 2
        then return Nothing
        else do
            unsafeWrite cp (vertexIndex lastCrossing) False
            (Just $!) `fmap` unsafeFreeze cp


analyseSymmetry :: (CrossingType ct) => Vertex Tangle ct -> (Vertex Tangle ct -> Bool) -> Maybe (Dn.DnSubGroup, (D4.D4, D4.D4), UArray Int Int)
analyseSymmetry lastCrossing skipCrossing = do
    let tangle = vertexOwner lastCrossing
        l = numberOfLegs tangle

        rootLegCCW = firstLeg tangle
        rootLegCW = opposite $ nthOutcomingDart lastCrossing 3
        (rootGCCW, rootCodeCCW) = rootCodeLegUnsafe rootLegCCW R.ccw
        (rootGCW, rootCodeCW) = rootCodeLegUnsafe rootLegCW R.cw

        compareRoots = compare rootCodeCCW rootCodeCW

        (rootG, rootCode, rootLeg, rootDir) =
            case compareRoots of
                GT -> (rootGCW, rootCodeCW, rootLegCW, R.cw)
                _  -> (rootGCCW, rootCodeCCW, rootLegCCW, R.ccw)

        rootDir' = R.oppositeDirection rootDir

    let skip dir leg =
            let c = beginVertex $ opposite leg
                c' = beginVertex $ opposite $ nextDir dir leg
            in skipCrossing c || (c == c')

    (period, periodG) <- fix (\ loop !li !leg ->
            case () of
                _ | li >= l           -> return (l, D4.i)
                  | skip rootDir' leg -> loop (li + 1) (nextCCW leg)
                  | otherwise         ->
                      let (g, cmp) = compareRootCodeUnsafe leg rootDir rootCode
                      in case cmp of
                          LT -> Nothing
                          GT -> loop (li + 1) (nextCCW leg)
                          EQ -> return (li, g D4.<*> D4.inverse rootG)
        ) 1 (nextCCW rootLeg)

    mirror <- case compareRoots of
        EQ -> return $ Just (legPlace rootLegCW, rootGCW D4.<*> D4.inverse rootGCCW)
        _  -> fix (\ loop !li !leg ->
                case () of
                    _ | li >= period     -> return Nothing
                      | skip rootDir leg -> loop (li + 1) (nextCCW leg)
                      | otherwise        ->
                          let (g, cmp) = compareRootCodeUnsafe leg rootDir' rootCode
                          in case cmp of
                              LT -> Nothing
                              GT -> loop (li + 1) (nextCCW leg)
                              EQ -> return $ Just (li + 2 * legPlace rootLeg, g D4.<*> D4.inverse rootG)
            ) 0 rootLeg

    return $ case mirror of
        Just (mirrorZ, mirrorG) -> (Dn.fromPeriodAndMirroredZero l period mirrorZ, (periodG, mirrorG), rootCode)
        Nothing                 -> (Dn.fromPeriod l period, (periodG, D4.i), rootCode)


compareRootCodeUnsafe :: (CrossingType ct) => Dart Tangle ct -> R.RotationDirection -> UArray Int Int -> (D4.D4, Ordering)
compareRootCodeUnsafe root dir rootCode =
    case globalTransformations tangle of
        Nothing      -> (D4.i, rawCompare)
        Just globals -> minimumBy (comparing snd) $ map (\ g -> (g, rawCompareWithGlobal g)) globals
    where
        tangle = dartOwner root
        n = numberOfVertices tangle

        rawCompareWithGlobal global = runST $ do
            x <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
            unsafeWrite x (vertexIndex $ endVertex root) 1
            q <- newArray_ (0, n - 1) :: ST s (STArray s Int (Dart Tangle ct))
            unsafeWrite q 0 (opposite root)
            free <- newSTRef 2

            let {-# INLINE look #-}
                look !d !s
                    | isLeg d    = return $! s `shiftL` 7
                    | otherwise  = do
                        let u = beginVertex d
                        ux <- unsafeRead x (vertexIndex u)
                        if ux > 0
                            then return $! ux + (s `shiftL` 7)
                            else do
                                nf <- readSTRef free
                                writeSTRef free $! nf + 1
                                unsafeWrite x (vertexIndex u) nf
                                unsafeWrite q (nf - 1) d
                                return $! nf + (s `shiftL` 7)

            let {-# INLINE bfs #-}
                bfs !h | h >= n     = return EQ
                       | otherwise  = do
                    d <- unsafeRead q h
                    nb <- foldMAdjacentDartsFrom d dir look 0
                    case crossingCodeWithGlobal global dir d of
                        (# be, le #) ->
                            case compare be (rootCode `unsafeAt` (2 * h)) of
                                EQ -> case compare (le + nb `shiftL` 3) (rootCode `unsafeAt` (2 * h + 1)) of
                                        EQ -> bfs $! h + 1
                                        cp -> return cp
                                cp -> return cp

            bfs 0

        rawCompare = runST $ do
            x <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
            unsafeWrite x (vertexIndex $ endVertex root) 1
            q <- newArray_ (0, n - 1) :: ST s (STArray s Int (Dart Tangle ct))
            unsafeWrite q 0 (opposite root)
            free <- newSTRef 2

            let {-# INLINE look #-}
                look !d !s
                    | isLeg d    = return $! s `shiftL` 7
                    | otherwise  = do
                        let u = beginVertex d
                        ux <- unsafeRead x (vertexIndex u)
                        if ux > 0
                            then return $! ux + (s `shiftL` 7)
                            else do
                                nf <- readSTRef free
                                writeSTRef free $! nf + 1
                                unsafeWrite x (vertexIndex u) nf
                                unsafeWrite q (nf - 1) d
                                return $! nf + (s `shiftL` 7)

            let {-# INLINE bfs #-}
                bfs !h | h >= n     = return EQ
                       | otherwise  = do
                    d <- unsafeRead q h
                    nb <- foldMAdjacentDartsFrom d dir look 0
                    case crossingCode dir d of
                        (# be, le #) ->
                            case compare be (rootCode `unsafeAt` (2 * h)) of
                                EQ -> case compare (le + nb `shiftL` 3) (rootCode `unsafeAt` (2 * h + 1)) of
                                        EQ -> bfs $! h + 1
                                        cp -> return cp
                                cp -> return cp

            bfs 0


rootCodeLegUnsafe :: (CrossingType ct) => Dart Tangle ct -> R.RotationDirection -> (D4.D4, UArray Int Int)
rootCodeLegUnsafe root dir =
    case globalTransformations tangle of
        Nothing      -> (D4.i, code)
        Just globals -> minimumBy (comparing snd) $ map (\ g -> (g, codeWithGlobal g)) globals
    where 
        tangle = dartOwner root
        n = numberOfVertices tangle

        codeWithGlobal global = runSTUArray $ do
            x <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
            unsafeWrite x (vertexIndex $ endVertex root) 1
            q <- newArray_ (0, n - 1) :: ST s (STArray s Int (Dart Tangle ct))
            unsafeWrite q 0 (opposite root)
            free <- newSTRef 2

            let {-# INLINE look #-}
                look !d !s
                    | isLeg d    = return $! s `shiftL` 7
                    | otherwise  = do
                        let u = beginVertex d
                        ux <- unsafeRead x (vertexIndex u)
                        if ux > 0
                            then return $! ux + (s `shiftL` 7)
                            else do
                                nf <- readSTRef free
                                writeSTRef free $! nf + 1
                                unsafeWrite x (vertexIndex u) nf
                                unsafeWrite q (nf - 1) d
                                return $! nf + (s `shiftL` 7)

            rc <- newArray (0, 2 * n - 1) 0 :: ST s (STUArray s Int Int)

            let {-# INLINE bfs #-}
                bfs !h = when (h < n) $ do
                    d <- unsafeRead q h
                    nb <- foldMAdjacentDartsFrom d dir look 0
                    case crossingCodeWithGlobal global dir d of
                        (# be, le #) -> do
                            unsafeWrite rc (2 * h) be
                            unsafeWrite rc (2 * h + 1) $! le + nb `shiftL` 3
                    bfs $! h + 1

            bfs 0
            return rc

        code = runSTUArray $ do
            x <- newArray (0, n) 0 :: ST s (STUArray s Int Int)
            unsafeWrite x (vertexIndex $ endVertex root) 1
            q <- newArray_ (0, n - 1) :: ST s (STArray s Int (Dart Tangle ct))
            unsafeWrite q 0 (opposite root)
            free <- newSTRef 2

            let {-# INLINE look #-}
                look !d !s
                    | isLeg d    = return $! s `shiftL` 7
                    | otherwise  = do
                        let u = beginVertex d
                        ux <- unsafeRead x (vertexIndex u)
                        if ux > 0
                            then return $! ux + (s `shiftL` 7)
                            else do
                                nf <- readSTRef free
                                writeSTRef free $! nf + 1
                                unsafeWrite x (vertexIndex u) nf
                                unsafeWrite q (nf - 1) d
                                return $! nf + (s `shiftL` 7)

            rc <- newArray (0, 2 * n - 1) 0 :: ST s (STUArray s Int Int)

            let {-# INLINE bfs #-}
                bfs !h = when (h < n) $ do
                    d <- unsafeRead q h
                    nb <- foldMAdjacentDartsFrom d dir look 0
                    case crossingCode dir d of
                        (# be, le #) -> do
                            unsafeWrite rc (2 * h) be
                            unsafeWrite rc (2 * h + 1) $! le + nb `shiftL` 3
                    bfs $! h + 1

            bfs 0
            return rc
