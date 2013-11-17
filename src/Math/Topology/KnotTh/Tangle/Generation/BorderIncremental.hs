{-# LANGUAGE UnboxedTuples, TypeFamilies #-}
module Math.Topology.KnotTh.Tangle.Generation.BorderIncremental
    ( module X
    , primeProjections
    , reducedProjections
    , templateProjections
    , primeIrreducibleDiagrams
    , primeIrreducibleDiagramsTriangle
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
import Control.Arrow (first)
import Control.Monad (when, guard)
import qualified Math.Algebra.RotationDirection as R
import qualified Math.Algebra.Group.Dn as Dn
import qualified Math.Algebra.Group.D4 as D4
import Math.Combinatorics.Generation.CanonicalConstructionPath as X
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Tangle.Generation.BorderIncremental.IncrementalTests


rootingTest :: (CrossingType ct) => Vertex Tangle ct -> Maybe (Dn.DnSubGroup, (D4.D4, D4.D4))
rootingTest lastCrossing = do
    let tangle = vertexOwner lastCrossing
    guard $ numberOfLegs tangle >= 4
    cp <- investigateConnectivity lastCrossing
    analyseSymmetry lastCrossing (unsafeAt cp . vertexIndex)


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


analyseSymmetry :: (CrossingType ct) => Vertex Tangle ct -> (Vertex Tangle ct -> Bool) -> Maybe (Dn.DnSubGroup, (D4.D4, D4.D4))
analyseSymmetry lastCrossing skipCrossing = do
    let tangle = vertexOwner lastCrossing
        l = numberOfLegs tangle

        rootLegCCW = firstLeg tangle
        rootLegCW = opposite $ nthOutcomingDart lastCrossing 3
        (rootGCCW, rootCodeCCW) = rootCodeLeg rootLegCCW R.ccw
        (rootGCW, rootCodeCW) = rootCodeLeg rootLegCW R.cw

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
                      let (g, cmp) = compareRootCode leg rootDir rootCode
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
                          let (g, cmp) = compareRootCode leg rootDir' rootCode
                          in case cmp of
                              LT -> Nothing
                              GT -> loop (li + 1) (nextCCW leg)
                              EQ -> return $ Just (li + 2 * legPlace rootLeg, g D4.<*> D4.inverse rootG)
            ) 0 rootLeg

    return $ case mirror of
        Just (mirrorZ, mirrorG) -> (Dn.fromPeriodAndMirroredZero l period mirrorZ, (periodG, mirrorG))
        Nothing                 -> (Dn.fromPeriod l period, (periodG, D4.i))


compareRootCode :: (CrossingType ct) => Dart Tangle ct -> R.RotationDirection -> UArray Int Int -> (D4.D4, Ordering)
compareRootCode leg dir rootCode =
    let (g, code) = rootCodeLeg leg dir
    in (g, compare code rootCode)


rootCodeLeg :: (CrossingType ct) => Dart Tangle ct -> R.RotationDirection -> (D4.D4, UArray Int Int)
rootCodeLeg !root !dir
    | isDart root                     = error "rootCodeLeg: leg expected"
    | numberOfFreeLoops tangle /= 0   = error "rootCodeLeg: free loops present"
    | numberOfVertices tangle > 127   = error "rootCodeLeg: too many crossings"
    | otherwise                       =
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


adjointDifferenceForBasis :: (D4.D4, D4.D4) -> Dn.DnSubGroup -> Dn.Dn -> Dn.Dn -> D4.D4
adjointDifferenceForBasis (adjRot, adjMir) sg a b
    | mod rotationDiff period /= 0  = error "adjointDifferenceForBasis: elements are not equivalent"
    | needMirror                    = adjRotation D4.<*> adjMir
    | otherwise                     = adjRotation
    where
        period = Dn.rotationPeriod sg

        needMirror = Dn.reflection a /= Dn.reflection b

        toRotate | needMirror  = Dn.reflectionBasis sg Dn.<*> b
                 | otherwise   = b

        rotationDiff = Dn.rotation a - Dn.rotation toRotate

        rotationNum =
            let d = rotationDiff `div` period
            in if Dn.reflection a then -d else d

        adjRotation | rotationNum >= 0  = iterate (D4.<*> adjRot) D4.e !! rotationNum
                    | otherwise         = iterate (D4.<*> D4.inverse adjRot) D4.e !! abs rotationNum


representativeGluingSitesEx' :: (CrossingType ct) => [ct] -> Int -> (Tangle ct, (Dn.DnSubGroup, (D4.D4, D4.D4))) -> [(Int, Dart Tangle ct, CrossingState ct)]
representativeGluingSitesEx' crossingsToGlue !gl (!tangle, (!symmetry, !adjBasis))
    | numberOfLegs tangle /= Dn.pointsUnderSubGroup symmetry  = error "gluingSites: different orders"
    | otherwise                                               = do
        let period = Dn.rotationPeriod symmetry

        (!legIndex, !inducedSymmetry) <-
            if not $ Dn.hasReflectionPart symmetry
                then [(x, Nothing) | x <- [0 .. period - 1]]
                else let mz = (Dn.mirroredZero symmetry + gl - 1) `mod` period

                         getEndpoint doubleIndex =
                             let legIndex = doubleIndex `quot` 2
                                 fixup = adjointDifferenceForBasis adjBasis symmetry
                                            (Dn.fromRotationReflection (numberOfLegs tangle) (legIndex - gl + 1, True))
                                            (Dn.fromRotation (numberOfLegs tangle) legIndex)
                                 induced | even doubleIndex  = Just $! fixup D4.<*> (case gl of { 3 -> D4.ec2 ; 2 -> D4.ec3 ; _ -> D4.e })
                                         | otherwise         = Nothing
                             in (legIndex, induced)

                         leftB = getEndpoint (mz - period)
                         rightB = getEndpoint mz

                         fill !c | c == fst rightB  = [rightB] -- sic!
                                 | c == fst leftB   = leftB : fill (c + 1)
                                 | otherwise        = (c, Nothing) : fill (c + 1)

                     in fill $ fst leftB

        let leg = nthLeg tangle legIndex
        cr <- crossingsToGlue
        state <- possibleOrientations cr inducedSymmetry
        return (gl, leg, state)


primeProjections, reducedProjections, templateProjections
    :: Int -> CanonicalConstructionPathI
        (Tangle ProjectionCrossing, Dn.DnSubGroup)
        (Int, Dart Tangle ProjectionCrossing, CrossingState ProjectionCrossing)
        (Vertex Tangle ProjectionCrossing, Dn.DnSubGroup)

primeProjections maxN =
    CanonicalConstructionPathClean
        { independentUpper = \ (tangle, symmetry) -> do
            guard $ numberOfVertices tangle < maxN
            let l = numberOfLegs tangle
            gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
            representativeGluingSitesEx' [ProjectionCrossing] gl (tangle, (symmetry, (D4.i, D4.i)))
        , tryAscent        = \ (gl, leg, st) -> do
            let root = glueToBorder leg gl st
            (sym, _) <- rootingTest root
            return (root, sym)
        , lowerProjection  = first vertexOwner
        , roots            = [(lonerProjection, Dn.fromPeriodAndMirroredZero 4 1 0)]
        }

reducedProjections maxN =
    filterUpper (\ _ (gl, leg, _) -> testNoMultiEdges leg gl) $
        primeProjections maxN


templateProjections maxN =
    filterUpper (\ (t, _) (gl, leg, _) -> (numberOfVertices t == 1 || numberOfLegs t > 4) && testNoMultiEdges leg gl) $
        filterLower (\ (gl, _, _) (root, _) -> gl < 3 || testFlow4 root) $
            primeProjections maxN


primeIrreducibleDiagrams, primeIrreducibleDiagramsTriangle
    :: Int -> CanonicalConstructionPathI
        (NATangle, (Dn.DnSubGroup, (D4.D4, D4.D4)))
        (Int, NATangleDart, ArbitraryCrossingState)
        (NATangle, (Dn.DnSubGroup, (D4.D4, D4.D4)))

primeIrreducibleDiagrams maxN =
    CanonicalConstructionPathClean
        { independentUpper = \ ts@(tangle, _) -> do
            guard $ numberOfVertices tangle < maxN
            let l = numberOfLegs tangle
            gl <- [1 .. min 3 $ min (l - 1) (l `div` 2)]
            representativeGluingSitesEx' [ArbitraryCrossing] gl ts
        , tryAscent        = \ (gl, leg, st) -> do
            guard $ testNo2ndReidemeisterReduction st leg gl
            let root = glueToBorder leg gl st
            sym <- rootingTest root
            return (vertexOwner root, sym)
        , lowerProjection  = id
        , roots            = [(lonerOverCrossingTangle, (Dn.fromPeriodAndMirroredZero 4 1 0, (D4.ec, D4.e)))]
        }


cutInTriangle :: Int -> (Int, Dart Tangle ct, a) -> Bool
cutInTriangle maxN (gl, leg, _) =
    let diagonalIndex n l = n + l `div` 2 - 2
        nextNumberOfLegs l = l + 4 - 2 * gl
        tangle = dartOwner leg
    in diagonalIndex (1 + numberOfVertices tangle) (nextNumberOfLegs $ numberOfLegs tangle) <= diagonalIndex maxN 4

{-
{-# INLINE nextNumberOfLegs #-}
nextNumberOfLegs :: Int -> Int -> Int
nextNumberOfLegs l gl = l + 4 - 2 * gl


{-# INLINE diagonalIndex #-}
diagonalIndex :: Int -> Int -> Int
diagonalIndex n l = n + l `div` 2 - 2
-}

primeIrreducibleDiagramsTriangle maxN =
    filterUpper (const $ cutInTriangle maxN) $ primeIrreducibleDiagrams maxN
