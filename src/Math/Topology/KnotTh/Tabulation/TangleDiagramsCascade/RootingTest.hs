{-# LANGUAGE MultiWayIf, UnboxedTuples #-}
module Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade.RootingTest
    ( rootingSymmetryTest
    , rootCodeLeg
    ) where

import Data.Function (fix)
import Data.Ord (comparing)
import Data.List (minimumBy)
import Data.Bits (shiftL)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (runST)
import Control.Monad (when, guard)
import Math.Topology.KnotTh.Algebra.Dihedral.Dn
import Math.Topology.KnotTh.Algebra.Dihedral.D4
import Math.Topology.KnotTh.Knotted.Crossings.SubTangle
import Math.Topology.KnotTh.Tangle


{-# SPECIALIZE rootingSymmetryTest :: Vertex Tangle ProjectionCrossing    -> Maybe (SubGroup Dn, (D4, D4), UV.Vector Int) #-}
{-# SPECIALIZE rootingSymmetryTest :: Vertex Tangle DiagramCrossing       -> Maybe (SubGroup Dn, (D4, D4), UV.Vector Int) #-}
{-# SPECIALIZE rootingSymmetryTest :: Vertex Tangle (SubTangleCrossing a) -> Maybe (SubGroup Dn, (D4, D4), UV.Vector Int) #-}
rootingSymmetryTest :: (Crossing a) => Vertex Tangle a -> Maybe (SubGroup Dn, (D4, D4), UV.Vector Int)
rootingSymmetryTest lastCrossing = do
    let tangle = vertexOwner lastCrossing
    guard $ numberOfLegs tangle >= 4
    cp <- investigateConnectivity lastCrossing
    analyseSymmetry lastCrossing cp


rootCodeLeg :: (Crossing a) => Dart Tangle a -> RotationDirection -> (D4, UV.Vector Int)
rootCodeLeg root dir
    | isDart root                     = error "rootCodeLeg: leg expected"
    | numberOfFreeLoops tangle /= 0   = error "rootCodeLeg: free loops present"
    | numberOfVertices tangle > 127   = error "rootCodeLeg: too many crossings"
    | otherwise                       = rootCodeLegUnsafe root dir
    where
        tangle = dartOwner root


investigateConnectivity :: Vertex Tangle a -> Maybe (UV.Vector Bool)
investigateConnectivity lastCrossing = runST $ do
    let tangle = vertexOwner lastCrossing
    let n = numberOfVertices tangle

    tins <- UMV.replicate (n + 1) (-1)
    cp <- UMV.replicate (n + 1) False
    timer <- newSTRef (1 :: Int)

    let {-# INLINE dfs #-}
        dfs !v !from = do
            tin <- readSTRef timer
            writeSTRef timer $! tin + 1
            UMV.unsafeWrite tins (vertexIndex v) tin

            let {-# INLINE walk #-}
                walk !d (!fup, !border)
                    | isLeg d    = return (fup, border + 1)
                    | u == from  = return (fup, border)
                    | otherwise  = do
                        utin <- UMV.unsafeRead tins (vertexIndex u)
                        if utin > 0
                            then return (min fup utin, border)
                            else do
                                (!thatFup, !thatBorder) <- dfs u v
                                when (thatFup >= tin) (UMV.unsafeWrite cp (vertexIndex v) True)
                                return (min fup thatFup, border + if thatFup <= tin then thatBorder else 1)
                    where
                        u = beginVertex d

            foldMIncomingDarts v walk (tin, 0 :: Int)

    (!_, !borderCut) <- dfs lastCrossing lastCrossing
    if borderCut <= 2
        then return Nothing
        else do
            UMV.unsafeWrite cp (vertexIndex lastCrossing) False
            (Just $!) `fmap` UV.unsafeFreeze cp


{-# SPECIALIZE analyseSymmetry :: Vertex Tangle ProjectionCrossing    -> UV.Vector Bool -> Maybe (SubGroup Dn, (D4, D4), UV.Vector Int) #-}
{-# SPECIALIZE analyseSymmetry :: Vertex Tangle DiagramCrossing       -> UV.Vector Bool -> Maybe (SubGroup Dn, (D4, D4), UV.Vector Int) #-}
{-# SPECIALIZE analyseSymmetry :: Vertex Tangle (SubTangleCrossing a) -> UV.Vector Bool -> Maybe (SubGroup Dn, (D4, D4), UV.Vector Int) #-}
analyseSymmetry :: (Crossing a) => Vertex Tangle a -> UV.Vector Bool -> Maybe (SubGroup Dn, (D4, D4), UV.Vector Int)
analyseSymmetry lastCrossing skipCrossing = do
    let tangle = vertexOwner lastCrossing
        l = numberOfLegs tangle

        rootLegCCW = firstLeg tangle
        rootLegCW = opposite $ nthOutcomingDart lastCrossing 3
        (rootGCCW, rootCodeCCW) = rootCodeLegUnsafe rootLegCCW ccw
        (rootGCW, rootCodeCW) = rootCodeLegUnsafe rootLegCW cw

        compareRoots = compare rootCodeCCW rootCodeCW

        (rootG, rootCode, rootLeg, rootDir) =
            case compareRoots of
                GT -> (rootGCW, rootCodeCW, rootLegCW, cw)
                _  -> (rootGCCW, rootCodeCCW, rootLegCCW, ccw)

        rootDir' = mirrorIt rootDir

    let skip dir leg =
            let v = endVertex leg
                v' = endVertex $ nextDir dir leg
            in (skipCrossing `UV.unsafeIndex` vertexIndex v) || (v == v')

    (period, periodG) <- fix (\ loop !li !leg ->
            if | li >= l           -> return (l, d4I)
               | skip rootDir' leg -> loop (li + 1) (nextCCW leg)
               | otherwise         ->
                   let (g, cmp) = compareRootCodeUnsafe leg rootDir rootCode
                   in case cmp of
                       LT -> Nothing
                       GT -> loop (li + 1) (nextCCW leg)
                       EQ -> return (li, g ∘ inverse rootG)
        ) 1 (nextCCW rootLeg)

    refl <- case compareRoots of
        EQ -> return $ Just (legPlace rootLegCW, rootGCW ∘ inverse rootGCCW)
        _  -> fix (\ loop !li !leg ->
                if | li >= period     -> return Nothing
                   | skip rootDir leg -> loop (li + 1) (nextCCW leg)
                   | otherwise        ->
                       let (g, cmp) = compareRootCodeUnsafe leg rootDir' rootCode
                       in case cmp of
                           LT -> Nothing
                           GT -> loop (li + 1) (nextCCW leg)
                           EQ -> return $ Just (li + 2 * legPlace rootLeg, g ∘ inverse rootG)
            ) 0 rootLeg

    return $ case refl of
        Just (mirrorZ, mirrorG) -> (fromPeriodAndMirroredZero l period mirrorZ, (periodG, mirrorG), rootCode)
        Nothing                 -> (fromPeriod l period, (periodG, d4I), rootCode)


{-# SPECIALIZE compareRootCodeUnsafe :: Dart Tangle ProjectionCrossing    -> RotationDirection -> UV.Vector Int -> (D4, Ordering) #-}
{-# SPECIALIZE compareRootCodeUnsafe :: Dart Tangle DiagramCrossing       -> RotationDirection -> UV.Vector Int -> (D4, Ordering) #-}
{-# SPECIALIZE compareRootCodeUnsafe :: Dart Tangle (SubTangleCrossing a) -> RotationDirection -> UV.Vector Int -> (D4, Ordering) #-}
compareRootCodeUnsafe :: (Crossing a) => Dart Tangle a -> RotationDirection -> UV.Vector Int -> (D4, Ordering)
compareRootCodeUnsafe root dir rootCode =
    case globalTransformations tangle of
        Nothing      -> (d4I, rawCompare)
        Just globals -> minimumBy (comparing snd) $ map (\ g -> (g, rawCompareWithGlobal g)) globals
    where
        tangle = dartOwner root
        n = numberOfVertices tangle

        rawCompareWithGlobal global = runST $ do
            x <- UMV.replicate (n + 1) 0
            UMV.unsafeWrite x (vertexIndex $ endVertex root) 1
            q <- MV.new n
            MV.unsafeWrite q 0 (opposite root)
            free <- newSTRef 2

            let {-# INLINE look #-}
                look !d !s
                    | isLeg d    = return $! s `shiftL` 7
                    | otherwise  = do
                        let u = beginVertex d
                        ux <- UMV.unsafeRead x (vertexIndex u)
                        if ux > 0
                            then return $! ux + (s `shiftL` 7)
                            else do
                                nf <- readSTRef free
                                writeSTRef free $! nf + 1
                                UMV.unsafeWrite x (vertexIndex u) nf
                                MV.unsafeWrite q (nf - 1) d
                                return $! nf + (s `shiftL` 7)

            let {-# INLINE bfs #-}
                bfs !h | h >= n     = return EQ
                       | otherwise  = do
                    d <- MV.unsafeRead q h
                    nb <- foldMIncomingDartsFrom d dir look 0
                    case crossingCodeWithGlobal global dir d of
                        (# be, le #) ->
                            case compare be (rootCode `UV.unsafeIndex` (2 * h)) of
                                EQ -> case compare (le + nb `shiftL` 3) (rootCode `UV.unsafeIndex` (2 * h + 1)) of
                                        EQ -> bfs $! h + 1
                                        cp -> return cp
                                cp -> return cp

            bfs 0

        rawCompare = runST $ do
            x <- UMV.replicate (n + 1) 0
            UMV.unsafeWrite x (vertexIndex $ endVertex root) 1
            q <- MV.new n
            MV.unsafeWrite q 0 (opposite root)
            free <- newSTRef 2

            let {-# INLINE look #-}
                look !d !s
                    | isLeg d    = return $! s `shiftL` 7
                    | otherwise  = do
                        let u = beginVertex d
                        ux <- UMV.unsafeRead x (vertexIndex u)
                        if ux > 0
                            then return $! ux + (s `shiftL` 7)
                            else do
                                nf <- readSTRef free
                                writeSTRef free $! nf + 1
                                UMV.unsafeWrite x (vertexIndex u) nf
                                MV.unsafeWrite q (nf - 1) d
                                return $! nf + (s `shiftL` 7)

            let {-# INLINE bfs #-}
                bfs !h | h >= n     = return EQ
                       | otherwise  = do
                    d <- MV.unsafeRead q h
                    nb <- foldMIncomingDartsFrom d dir look 0
                    case crossingCode dir d of
                        (# be, le #) ->
                            case compare be (rootCode `UV.unsafeIndex` (2 * h)) of
                                EQ -> case compare (le + nb `shiftL` 3) (rootCode `UV.unsafeIndex` (2 * h + 1)) of
                                        EQ -> bfs $! h + 1
                                        cp -> return cp
                                cp -> return cp

            bfs 0


{-# SPECIALIZE rootCodeLegUnsafe :: Dart Tangle ProjectionCrossing    -> RotationDirection -> (D4, UV.Vector Int) #-}
{-# SPECIALIZE rootCodeLegUnsafe :: Dart Tangle DiagramCrossing       -> RotationDirection -> (D4, UV.Vector Int) #-}
{-# SPECIALIZE rootCodeLegUnsafe :: Dart Tangle (SubTangleCrossing a) -> RotationDirection -> (D4, UV.Vector Int) #-}
rootCodeLegUnsafe :: (Crossing a) => Dart Tangle a -> RotationDirection -> (D4, UV.Vector Int)
rootCodeLegUnsafe root dir =
    case globalTransformations tangle of
        Nothing      -> (d4I, code)
        Just globals -> minimumBy (comparing snd) $ map (\ g -> (g, codeWithGlobal g)) globals
    where 
        tangle = dartOwner root
        n = numberOfVertices tangle

        codeWithGlobal global = UV.create $ do
            x <- UMV.replicate (n + 1) 0
            UMV.unsafeWrite x (vertexIndex $ endVertex root) 1
            q <- MV.new n
            MV.unsafeWrite q 0 (opposite root)
            free <- newSTRef 2

            let {-# INLINE look #-}
                look !d !s
                    | isLeg d    = return $! s `shiftL` 7
                    | otherwise  = do
                        let u = beginVertex d
                        ux <- UMV.unsafeRead x (vertexIndex u)
                        if ux > 0
                            then return $! ux + (s `shiftL` 7)
                            else do
                                nf <- readSTRef free
                                writeSTRef free $! nf + 1
                                UMV.unsafeWrite x (vertexIndex u) nf
                                MV.unsafeWrite q (nf - 1) d
                                return $! nf + (s `shiftL` 7)

            rc <- UMV.replicate (2 * n) 0

            let {-# INLINE bfs #-}
                bfs !h = when (h < n) $ do
                    d <- MV.unsafeRead q h
                    nb <- foldMIncomingDartsFrom d dir look 0
                    case crossingCodeWithGlobal global dir d of
                        (# be, le #) -> do
                            UMV.unsafeWrite rc (2 * h) be
                            UMV.unsafeWrite rc (2 * h + 1) $! le + nb `shiftL` 3
                    bfs $! h + 1

            bfs 0
            return rc

        code = UV.create $ do
            x <- UMV.replicate (n + 1) 0
            UMV.unsafeWrite x (vertexIndex $ endVertex root) 1
            q <- MV.new n
            MV.unsafeWrite q 0 (opposite root)
            free <- newSTRef 2

            let {-# INLINE look #-}
                look !d !s
                    | isLeg d    = return $! s `shiftL` 7
                    | otherwise  = do
                        let u = beginVertex d
                        ux <- UMV.unsafeRead x (vertexIndex u)
                        if ux > 0
                            then return $! ux + (s `shiftL` 7)
                            else do
                                nf <- readSTRef free
                                writeSTRef free $! nf + 1
                                UMV.unsafeWrite x (vertexIndex u) nf
                                MV.unsafeWrite q (nf - 1) d
                                return $! nf + (s `shiftL` 7)

            rc <- UMV.replicate (2 * n) 0

            let {-# INLINE bfs #-}
                bfs !h = when (h < n) $ do
                    d <- MV.unsafeRead q h
                    nb <- foldMIncomingDartsFrom d dir look 0
                    case crossingCode dir d of
                        (# be, le #) -> do
                            UMV.unsafeWrite rc (2 * h) be
                            UMV.unsafeWrite rc (2 * h + 1) $! le + nb `shiftL` 3
                    bfs $! h + 1

            bfs 0
            return rc
