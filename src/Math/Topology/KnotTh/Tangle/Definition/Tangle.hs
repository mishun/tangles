{-# LANGUAGE TemplateHaskell, TypeFamilies, UnboxedTuples #-}
module Math.Topology.KnotTh.Tangle.Definition.Tangle
    ( Tangle
    , TangleProjection
    , TangleProjectionVertex
    , TangleProjectionDart
    , TangleDiagram
    , TangleDiagramVertex
    , TangleDiagramDart
    ) where

import Language.Haskell.TH
import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Data.Bits ((.&.), complement, shiftL)
import Data.List (nub, sort, foldl')
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Primitive as PV
import qualified Data.Vector.Primitive.Mutable as PMV
import Data.Array.IArray ((!), listArray)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST, runST)
import Control.Monad (void, forM_, when, unless, foldM_, foldM, filterM)
import Text.Printf
import qualified Math.Algebra.Group.D4 as D4
import qualified Math.Algebra.RotationDirection as R
import Math.Topology.KnotTh.Knotted.TH.Knotted
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Crossings.Projection
import Math.Topology.KnotTh.Crossings.Diagram
import Math.Topology.KnotTh.Tangle.Definition.TangleLike


produceKnotted
    [d| data Tangle ct =
            Tangle
                { legsCount :: {-# UNPACK #-} !Int
                }

        instance Knotted Tangle where
            vertexCrossing = undefined
            numberOfFreeLoops = undefined
            changeNumberOfFreeLoops = undefined
            emptyKnotted = undefined
            implode = undefined

            type ExplodeType Tangle a = (Int, [(Int, Int)], [([(Int, Int)], a)])

            explode tangle =
                ( numberOfFreeLoops tangle
                , map endPair' $ allLegs tangle
                , map (\ v -> (map endPair' $ outcomingDarts v, vertexCrossing v)) $ allVertices tangle
                )

            homeomorphismInvariant tangle
                | n > 127    = error $ printf "homeomorphismInvariant: too many crossings (%i)" n
                | otherwise  = minimum $ do
                    leg <- allLegs tangle
                    dir <- R.bothDirections
                    globalG <- fromMaybe [D4.i] $ globalTransformations tangle
                    return $ code globalG dir leg
                where
                    n = numberOfVertices tangle
                    l = numberOfLegs tangle

                    code globalG dir leg = PV.create $ do
                        index <- PMV.replicate (n + 1) 0
                        queue <- MV.new n
                        free <- newSTRef 1

                        let {-# INLINE look #-}
                            look !d
                                | isLeg d    = return 0
                                | otherwise  = do
                                    let u = beginVertex d
                                    ux <- PMV.unsafeRead index $! vertexIndex u
                                    if ux > 0
                                        then return $! ux
                                        else do
                                            nf <- readSTRef free
                                            writeSTRef free $! nf + 1
                                            PMV.unsafeWrite index (vertexIndex u) nf
                                            MV.unsafeWrite queue (nf - 1) d
                                            return $! nf

                            {-# INLINE lookAndAdd #-}
                            lookAndAdd !d !s = do
                                !c <- look d
                                return $! c + s `shiftL` 7

                        rc <- PMV.new (l + 2 * n + 1)
                        PMV.unsafeWrite rc 0 $! numberOfFreeLoops tangle
                        foldM_ (\ !d !i -> do
                                look (opposite d) >>= PMV.unsafeWrite rc i
                                return $! nextDir dir d
                            ) leg [1 .. l]

                        let bfs !st !headI = do
                                tailI <- readSTRef free
                                if headI >= tailI - 1 then return $! st else do
                                    input <- MV.unsafeRead queue headI
                                    !nb <- foldMAdjacentDartsFrom input dir lookAndAdd 0
                                    case crossingCodeWithGlobal globalG dir input of
                                        (# be, le'' #) -> do
                                            let le = le'' + shiftL nb 3
                                                bi = l + 1 + 2 * headI
                                                li = bi + 1
                                            case st of
                                                LT -> PMV.unsafeWrite rc bi be >> PMV.unsafeWrite rc li le >> bfs LT (headI + 1)
                                                EQ -> do
                                                    be' <- PMV.unsafeRead rc bi
                                                    case compare be be' of
                                                        LT -> PMV.unsafeWrite rc bi be >> PMV.unsafeWrite rc li le >> bfs LT (headI + 1)
                                                        EQ -> do
                                                            le' <- PMV.unsafeRead rc li
                                                            case compare le le' of
                                                                LT -> PMV.unsafeWrite rc li le >> bfs LT (headI + 1)
                                                                EQ -> bfs EQ (headI + 1)
                                                                GT -> return GT
                                                        GT -> return GT
                                                GT -> return GT

                        LT <- bfs LT 0
                        fix $ \ recheck -> do
                            tailI <- readSTRef free
                            when (tailI <= n) $ do
                                notVisited <- filterM (\ !i -> (== 0) `fmap` PMV.unsafeRead index i) [1 .. n]

                                (d, _) <- foldM (\ (pd, !st) !d -> do
                                        writeSTRef free tailI
                                        forM_ notVisited $ \ !i -> PMV.unsafeWrite index i 0
                                        void $ look d
                                        r <- bfs st (tailI - 1)
                                        return $! case r of
                                            LT -> (d, EQ)
                                            _  -> (pd, EQ)
                                    )
                                    (undefined, LT)
                                    [d | i <- notVisited, d <- outcomingDarts (nthVertex tangle i)]

                                writeSTRef free tailI
                                forM_ notVisited $ \ !i -> PMV.unsafeWrite index i 0
                                void $ look d
                                LT <- bfs LT (tailI - 1)
                                recheck

                        return rc

            isConnected tangle
                | numberOfEdges tangle == 0 && numberOfFreeLoops tangle <= 1  = True
                | numberOfFreeLoops tangle /= 0                               = False
                | otherwise                                                   = all (\ (a, b) -> S.member a con && S.member b con) edges
                where
                    edges = allEdges tangle
                    con = dfs S.empty $ fst $ head edges
                    dfs vis c | S.member c vis  = vis
                              | otherwise       = foldl' dfs (S.insert c vis) neigh
                        where
                            neigh | isLeg c    = [opposite c]
                                  | otherwise  = [opposite c, nextCCW c, nextCW c]

    |] $
    let legsCount = varE $ mkName "legsCount"
        dart = conE $ mkName "Dart"
    in defaultKnotted
        { implodeExplodeSettings =
            let lN = mkName "l"
                l = varE lN
                brdN = mkName "brd"
                brd = varE brdN
            in defaultImplodeExplode
                { extraImplodeExplodeParams =
                    [ (brdN, [t| [(Int, Int)] |], \ t -> [| map endPair' $ allLegs $t |])
                    ]

                , extraImplodePairCases =
                    [ \ spliceError n c p ->
                        ([| $c == (0 :: Int) |],
                            [|  if $p >= (0 :: Int) && $p < $l
                                    then 4 * $n + $p :: Int
                                    else $(spliceError "leg index %i is out of bounds [0 .. %i]" [ p, [| $l - 1 :: Int |] ])
                            |])
                    ]

                , extraExplodePairCases =
                    [ \ d -> ([| isLeg $d |], [| (,) (0 :: Int) $! legPlace $d |])
                    ]

                , modifyImplodeLimit = Just $ \ n _ -> [| 4 * $n + $l - 1 :: Int |]

                , implodePreExtra = \ spliceError ->
                    [ letS $ (:[]) $ valD (varP lN) (normalB [| length $brd |]) []
                    , noBindS
                        [|  when (odd ($l :: Int)) $
                                $(spliceError "number of legs %i must be even" [l])
                        |]
                    ]

                , implodePostExtra = \ n _ spliceFill ->
                    [ noBindS
                        [|  forM_ (zip $brd [0 :: Int ..]) $ \ ((!c, !p), !i) ->
                                let a = 4 * $n + i
                                in $(spliceFill [| a |] ([| c |], [| p |]))
                        |]
                    ]

                , implodeInitializers =
                    [ (,) (mkName "legsCount") `fmap` l
                    ]
                }

        , modifyNumberOfEdges = Just $ \ t _ ->
            [| 2 * numberOfVertices $t + ($legsCount $t `div` 2) |]

        , modifyIsDart = Just $ \ (t, i) ->
            [| $i < 4 * numberOfVertices $t |]

        , modifyNextCCW = Just $ \ (t, d) e ->
            [|  let n = 4 * numberOfVertices $t
                in if $d >= n
                    then $dart $t $! n + ($d - n + 1) `mod` $legsCount $t
                    else $e
            |]

        , modifyNextCW = Just $ \ (t, d) e ->
            [|  let n = 4 * numberOfVertices $t
                in if $d >= n
                    then $dart $t $! n + ($d - n - 1) `mod` $legsCount $t
                    else $e
            |]

        , modifyDartPlace = Just $ \ (t, d) e ->
            [|  let n = 4 * numberOfVertices $t
                in if $d >= n
                    then error $ printf "dartPlace: taken from %i-th leg" ($d - n)
                    else $e
            |]

        , modifyIncidentCrossing = Just $ \ (t, d) e ->
            [|  let n = 4 * numberOfVertices $t
                in if $d >= n
                    then error $ printf "incidentCrossing: taken from %i-th leg" ($d - n)
                    else $e
            |]

        , modifyFoldMIncidentDartsFrom = Just $ \ (d, _) e ->
            [|  if isDart $d
                    then $e
                    else error $ printf "foldMIncidentDartsFrom: taken from leg %i" (legPlace $d)
            |]

        , emptyExtraInitializers =
            [ (,) (mkName "legsCount") `fmap` [| 0 :: Int |]
            ]
        }


instance PlanarAlgebra Tangle where
    numberOfLegs = legsCount

    nthLeg t i | l == 0     = error "nthLeg: tangle has no legs"
               | otherwise  = Dart t (n + i `mod` l)
        where
            l = numberOfLegs t
            n = 4 * numberOfVertices t

    allLegs t =
        let n = 4 * numberOfVertices t
            l = numberOfLegs t
        in map (Dart t) [n .. n + l - 1]

    legPlace d@(Dart t i) | isDart d   = error $ printf "legPlace: taken from non-leg %s" $ show d
                          | otherwise  = i - 4 * numberOfVertices t

    isLeg = not . isDart


instance TangleLike Tangle where
    zeroTangle =
        Tangle
            { loopsCount      = 0
            , vertexCount     = 0
            , involutionArray = PV.fromList [3, 2, 1, 0]
            , crossingsArray  = V.empty
            , legsCount       = 4
            }

    infinityTangle =
        Tangle
            { loopsCount      = 0
            , vertexCount     = 0
            , involutionArray = PV.fromList [1, 0, 3, 2]
            , crossingsArray  = V.empty
            , legsCount       = 4
            }

    identityTangle =
        Tangle
            { loopsCount      = 0
            , vertexCount     = 0
            , involutionArray = PV.fromList [1, 0]
            , crossingsArray  = V.empty
            , legsCount       = 2
            }

    lonerTangle cr =
        Tangle
            { loopsCount      = 0
            , vertexCount     = 1
            , involutionArray = PV.fromList [4, 5, 6, 7, 0, 1, 2, 3]
            , crossingsArray  = V.singleton cr
            , legsCount       = 4
            }

    rotateTangle !rot tangle
        | l == 0 || rot == 0  = tangle
        | otherwise           =
            tangle
                { involutionArray = PV.create $ do
                    let n = 4 * numberOfVertices tangle
                        a = involutionArray tangle
                        modify i | i < n      = i
                                 | otherwise  = n + mod (i - n + rot) l
                    a' <- PMV.new (n + l)
                    forM_ [0 .. n - 1] $ \ !i ->
                        PMV.unsafeWrite a' i $ modify (a `PV.unsafeIndex` i)
                    forM_ [0 .. l - 1] $ \ !i ->
                        PMV.unsafeWrite a' (n + mod (i + rot) l) $ modify (a `PV.unsafeIndex` (n + i))
                    return a'
                }
        where
            l = numberOfLegs tangle

    mirrorTangleWith f tangle =
        tangle
            { involutionArray = PV.create $ do
                let l = numberOfLegs tangle
                    n = 4 * numberOfVertices tangle
                    a = involutionArray tangle
                    modify i | i < n      = (i .&. complement 3) + ((-i) .&. 3)
                             | otherwise  = n + mod (n - i) l
                a' <- PMV.new (n + l)
                forM_ [0 .. n + l - 1] $ \ !i ->
                    PMV.unsafeWrite a' (modify i) $ modify (a `PV.unsafeIndex` i)
                return a'
            , crossingsArray = fmap f $ crossingsArray tangle
            }

    glueTangles legsToGlue legA legB = runST $ do
        unless (isLeg legA) $
            fail $ printf "glueTangles: first leg parameter %s is not a leg" (show legA)
        unless (isLeg legB) $
            fail $ printf "glueTangles: second leg parameter %s is not a leg" (show legB)

        let tangleA = dartOwner legA
            lA = numberOfLegs tangleA
            nA = numberOfVertices tangleA
            lpA = legPlace legA

            tangleB = dartOwner legB
            lB = numberOfLegs tangleB
            nB = numberOfVertices tangleB
            lpB = legPlace legB

        when (legsToGlue < 0 || legsToGlue > min lA lB) $
            fail $ printf "glueTangles: number of legs to glue %i is out of bound" legsToGlue

        let newL = lA + lB - 2 * legsToGlue
            newC = nA + nB

        visited <- MV.replicate legsToGlue False

        cr <- do
            let {-# INLINE convertA #-}
                convertA !x
                    | x < 4 * nA        = return $! x
                    | ml >= legsToGlue  = return $! 4 * newC + ml - legsToGlue
                    | otherwise         = do
                        MV.unsafeWrite visited ml True
                        convertB (involutionArray tangleB `PV.unsafeIndex` (4 * nB + (lpB - ml) `mod` lB))
                    where
                        ml = (x - 4 * nA - lpA) `mod` lA

                {-# INLINE convertB #-}
                convertB !x
                    | x < 4 * nB            = return $! 4 * nA + x
                    | ml < lB - legsToGlue  = return $! 4 * newC + ml + lA - legsToGlue
                    | otherwise             = do
                        MV.unsafeWrite visited (lB - ml - 1) True
                        convertA (involutionArray tangleA `PV.unsafeIndex` (4 * nA + (lpA + lB - ml - 1) `mod` lA))
                    where
                        ml = (x - 4 * nB - lpB - 1) `mod` lB

            cr <- PMV.new (4 * newC + newL) :: ST s (PMV.STVector s Int)
            forM_ [0 .. 4 * nA - 1] $ \ !i ->
                convertA (involutionArray tangleA `PV.unsafeIndex` i)
                    >>= PMV.unsafeWrite cr i

            forM_ [0 .. 4 * nB - 1] $ \ !i ->
                convertB (involutionArray tangleB `PV.unsafeIndex` i)
                    >>= PMV.unsafeWrite cr (4 * nA + i)

            forM_ [0 .. lA - legsToGlue - 1] $ \ !i ->
                convertA (involutionArray tangleA `PV.unsafeIndex` (4 * nA + (lpA + legsToGlue + i) `mod` lA))
                    >>= PMV.unsafeWrite cr (4 * newC + i)

            forM_ [0 .. lB - legsToGlue - 1] $ \ !i ->
                convertB (involutionArray tangleB `PV.unsafeIndex` (4 * nB + (lpB + 1 + i) `mod` lB))
                    >>= PMV.unsafeWrite cr (4 * newC + lA - legsToGlue + i) 
            PV.unsafeFreeze cr

        extraLoops <- do
            let markA a = do
                    let ai = 4 * nA + (lpA + a) `mod` lA
                        bi = involutionArray tangleA `PV.unsafeIndex` ai
                        b = (bi - 4 * nA - lpA) `mod` lA
                    v <- MV.unsafeRead visited b
                    unless v $ MV.unsafeWrite visited b True >> markB b

                markB a = do
                    let ai = 4 * nB + (lpB - a) `mod` lB
                        bi = involutionArray tangleB `PV.unsafeIndex` ai
                        b = (lpB - (bi - 4 * nB)) `mod` lB
                    v <- MV.unsafeRead visited b
                    unless v $ MV.unsafeWrite visited b True >> markA b

            foldM (\ !s !i -> do
                    v <- MV.unsafeRead visited i
                    if v
                        then return $! s
                        else markA i >> (return $! s + 1)
                ) 0 [0 .. legsToGlue - 1]

        return Tangle
            { loopsCount      = numberOfFreeLoops tangleA + numberOfFreeLoops tangleB + extraLoops
            , vertexCount     = newC
            , involutionArray = cr
            , crossingsArray  = crossingsArray tangleA V.++ crossingsArray tangleB
            , legsCount       = newL
            }

    glueToBorder leg legsToGlue !crossingToGlue = runST $ do
        unless (isLeg leg) $
            fail $ printf "glueToBorder: leg expected, but %s received" (show leg)

        when (legsToGlue < 0 || legsToGlue > 4) $
            fail $ printf "glueToBorder: legsToGlue must be in [0 .. 4], but %i found" legsToGlue

        let tangle = dartOwner leg
            oldL = numberOfLegs tangle
        when (oldL < legsToGlue) $
            fail $ printf "glueToBorder: not enough legs to glue (l = %i, legsToGlue = %i)" oldL legsToGlue

        let oldC = numberOfVertices tangle
            newC = oldC + 1
            newL = oldL + 4 - 2 * legsToGlue
            lp = legPlace leg 

        let result = Tangle
                { loopsCount      = numberOfFreeLoops tangle
                , vertexCount     = newC
                , involutionArray = PV.create $ do
                    cr <- PMV.new (4 * newC + newL) :: ST s (PMV.STVector s Int)

                    let {-# INLINE copyModified #-}
                        copyModified !index !index' =
                            let y | x < 4 * oldC            = x
                                  | ml < oldL - legsToGlue  = 4 * newC + 4 - legsToGlue + ml
                                  | otherwise               = 4 * newC - 5 + oldL - ml
                                  where
                                      x = involutionArray tangle `PV.unsafeIndex` index'
                                      ml = (x - 4 * oldC - lp - 1) `mod` oldL
                            in PMV.unsafeWrite cr index y

                    forM_ [0 .. 4 * oldC - 1] $ \ !i ->
                        copyModified i i

                    forM_ [0 .. legsToGlue - 1] $ \ !i ->
                        copyModified (4 * (newC - 1) + i) (4 * oldC + ((lp - i) `mod` oldL))

                    forM_ [0 .. 3 - legsToGlue] $ \ !i -> do
                        let a = 4 * (newC - 1) + legsToGlue + i
                            b = 4 * newC + i
                        PMV.unsafeWrite cr a b
                        PMV.unsafeWrite cr b a

                    forM_ [0 .. oldL - 1 - legsToGlue] $ \ !i ->
                        copyModified (4 * newC + i + 4 - legsToGlue) (4 * oldC + ((lp + 1 + i) `mod` oldL))

                    return $! cr

                , crossingsArray  = V.snoc (crossingsArray tangle) crossingToGlue
                , legsCount       = newL
                }

        return $! nthVertex result newC 

    tensorSubst k crossF tangle = implode (k * numberOfFreeLoops tangle, border, body)
        where
            n = numberOfVertices tangle

            crossSubst = (listArray (1, n) :: [a] -> Array Int a) $ do
                c <- allVertices tangle
                let t = crossF c
                when (numberOfLegs t /= 4 * k) $
                    fail "bad number of legs"
                return $! t

            crossOffset = (listArray (1, n) :: [Int] -> UArray Int Int) $
                scanl (\ !p !i -> p + numberOfVertices (crossSubst ! i)) 0 [1 .. n]

            resolveInCrossing !v !d
                | isLeg d    =
                    let p = legPlace d
                    in resolveOutside (opposite $ nthOutcomingDart v $ p `div` k) (p `mod` k)
                | otherwise  =
                    let (c, p) = beginPair' d
                    in ((crossOffset ! vertexIndex v) + c, p)

            resolveOutside !d !i
                | isLeg d    = (0, k * legPlace d + i)
                | otherwise  =
                    let (c, p) = beginPair d
                    in resolveInCrossing c $ opposite $ nthLeg (crossSubst ! vertexIndex c) (k * p + k - 1 - i)

            border = do
                d <- allLegOpposites tangle
                i <- [0 .. k - 1]
                return $! resolveOutside d $ k - 1 - i

            body = do
                c <- allVertices tangle
                let t = crossSubst ! vertexIndex c
                c' <- allVertices t
                return (map (resolveInCrossing c) $ incomingDarts c', vertexCrossing c')


instance Show (Dart Tangle a) where
    show d | isLeg d    = printf "(Leg %i)" $ legPlace d
           | otherwise  = let (c, p) = beginPair' d
                          in printf "(Dart %i %i)" c p


instance KnottedWithPrimeTest Tangle where
    isPrime tangle = connections == nub connections
        where
            idm = let faces = directedPathsDecomposition (nextCW, nextCCW)
                  in M.fromList $ concatMap (\ (face, i) -> zip face $ repeat i) $ zip faces [(0 :: Int) ..]

            connections =
                let getPair (da, db) =
                        let a = idm M.! da
                            b = idm M.! db
                        in (min a b, max a b)
                in sort $ map getPair $ allEdges tangle

            directedPathsDecomposition continue =
                let processDart (paths, s) d
                        | S.member d s  = (paths, s)
                        | otherwise     = (path : paths, nextS)
                        where
                            path = containingDirectedPath continue d
                            nextS = foldl' (flip S.insert) s path
                in fst $ foldl' processDart ([], S.empty) $ allHalfEdges tangle

            containingDirectedPath (adjForward, adjBackward) start
                | isCycle    = forward
                | otherwise  = walkBackward (start, forward)
                where
                    (forward, isCycle) = walkForward start

                    walkForward d
                        | isLeg opp     = ([d], False)
                        | start == nxt  = ([d], True)
                        | otherwise     = (d : nextPath, nextCycle)
                        where
                            opp = opposite d
                            nxt = adjForward opp
                            (nextPath, nextCycle) = walkForward nxt

                    walkBackward (d, path)
                        | isLeg d    = path
                        | otherwise  = let prev = opposite $ adjBackward d in walkBackward (prev, prev : path)


type TangleProjection = Tangle ProjectionCrossing
type TangleProjectionVertex = Vertex Tangle ProjectionCrossing
type TangleProjectionDart = Dart Tangle ProjectionCrossing


type TangleDiagram = Tangle DiagramCrossing
type TangleDiagramVertex = Vertex Tangle DiagramCrossing
type TangleDiagramDart = Dart Tangle DiagramCrossing
