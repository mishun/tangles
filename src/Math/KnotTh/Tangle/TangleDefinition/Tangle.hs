{-# LANGUAGE TemplateHaskell, TypeFamilies #-}
module Math.KnotTh.Tangle.TangleDefinition.Tangle
    ( module Math.KnotTh.Knotted
    , Tangle
    , crossingTangle
    , dartTangle
    , emptyTangle
    , identityTangle
    , zeroTangle
    , infinityTangle
    , lonerTangle
    , glueToBorder
    , glueTangles
    , changeNumberOfFreeLoops
    ) where

import Language.Haskell.TH
import Data.List (nub, sort, foldl')
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Array.Base (listArray, newArray, newArray_, readArray, unsafeAt, unsafeWrite)
import Data.Array.ST (STArray, STUArray)
import Data.Array.Unsafe (unsafeFreeze)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_, when, unless, foldM)
import Text.Printf
import Math.KnotTh.Knotted.TH.Knotted
import Math.KnotTh.Knotted.TH.Show
import Math.KnotTh.Knotted
import Math.KnotTh.Tangle.TangleDefinition.TangleLike


produceKnotted
    [d| data Tangle ct = Tangle { legsCount :: {-# UNPACK #-} !Int } |] $
    let legsCount = varE $ mkName "legsCount"
        dart = conE $ mkName "Dart"
    in defaultKnotted
        { implodeExplodeSettings = Just $
            let lN = mkName "l"
                l = varE lN
                brdN = mkName "brd"
                brd = varE brdN
            in defaultImplodeExplode
                { extraImplodeExplodeParams =
                    [ (brdN, [t| [(Int, Int)] |], \ t -> [| map (toPair . opposite) $ allLegs $t |])
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

                , implodePostExtra = \ n spliceFill ->
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
            [| 2 * numberOfCrossings $t + ($legsCount $t `div` 2) |]

        , modifyIsDart = Just $ \ (t, i) ->
            [| $i < 4 * numberOfCrossings $t |]

        , modifyNextCCW = Just $ \ (t, d) e ->
            [|  let n = 4 * numberOfCrossings $t
                in if $d >= n
                    then $dart $t $! n + ($d - n + 1) `mod` $legsCount $t
                    else $e
            |]

        , modifyNextCW = Just $ \ (t, d) e ->
            [|  let n = 4 * numberOfCrossings $t
                in if $d >= n
                    then $dart $t $! n + ($d - n - 1) `mod` $legsCount $t
                    else $e
            |]

        , modifyDartPlace = Just $ \ (t, d) e ->
            [|  let n = 4 * numberOfCrossings $t
                in if $d >= n
                    then error $ printf "dartPlace: taken from %i-th leg" ($d - n)
                    else $e
            |]

        , modifyIncidentCrossing = Just $ \ (t, d) e ->
            [|  let n = 4 * numberOfCrossings $t
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


instance TangleLike Tangle where
    numberOfLegs = legsCount

    allLegs t =
        let n = 4 * numberOfCrossings t
            l = numberOfLegs t
        in map (Dart t) [n .. n + l - 1]

    nthLeg t i
        | l == 0     = error "nthLeg: tangle has no legs"
        | otherwise  = Dart t $! n + i `mod` l
        where
            l = numberOfLegs t
            n = 4 * numberOfCrossings t

    isLeg = not . isDart

    legPlace d@(Dart t i)
        | isDart d   = error $ printf "legPlace: taken from non-leg %s" $ show d
        | otherwise  = i - 4 * numberOfCrossings t

    identityTangle = Tangle
        { loopsCount = 0
        , crossCount = 0
        , crossArray = listArray (0, 1) [1, 0]
        , stateArray = listArray (0, -1) []
        , legsCount  = 2
        }

    glueTangles legsToGlue legA legB = runST $ do
        unless (isLeg legA) $ fail $
            printf "glueTangles: first leg parameter %s is not a leg" (show legA)
        unless (isLeg legB) $ fail $
            printf "glueTangles: second leg parameter %s is not a leg" (show legB)

        let tangleA = dartTangle legA
            lA = numberOfLegs tangleA
            nA = numberOfCrossings tangleA
            lpA = legPlace legA

            tangleB = dartTangle legB
            lB = numberOfLegs tangleB
            nB = numberOfCrossings tangleB
            lpB = legPlace legB

        when (legsToGlue < 0 || legsToGlue > min lA lB) $ fail $
            printf "glueTangles: number of legs to glue %i is out of bound" legsToGlue

        let newL = lA + lB - 2 * legsToGlue
            newC = nA + nB

        visited <- newArray (0, legsToGlue - 1) False :: ST s (STUArray s Int Bool)

        cr <- do
            let {-# INLINE convertA #-}
                convertA !x
                    | x < 4 * nA        = return $! x
                    | ml >= legsToGlue  = return $! 4 * newC + ml - legsToGlue
                    | otherwise         = unsafeWrite visited ml True >> convertB (crossArray tangleB `unsafeAt` (4 * nB + (lpB - ml) `mod` lB))
                    where
                        ml = (x - 4 * nA - lpA) `mod` lA

                {-# INLINE convertB #-}
                convertB !x
                    | x < 4 * nB            = return $! 4 * nA + x
                    | ml < lB - legsToGlue  = return $! 4 * newC + ml + lA - legsToGlue
                    | otherwise             = unsafeWrite visited (lB - ml - 1) True >> convertA (crossArray tangleA `unsafeAt` (4 * nA + (lpA + lB - ml - 1) `mod` lA))
                    where
                        ml = (x - 4 * nB - lpB - 1) `mod` lB

            cr <- newArray_ (0, 4 * newC + newL - 1) :: ST s (STUArray s Int Int)
            forM_ [0 .. 4 * nA - 1] $ \ !i ->
                convertA (crossArray tangleA `unsafeAt` i)
                    >>= unsafeWrite cr i
            forM_ [0 .. 4 * nB - 1] $ \ !i ->
                convertB (crossArray tangleB `unsafeAt` i)
                    >>= unsafeWrite cr (4 * nA + i)
            forM_ [0 .. lA - legsToGlue - 1] $ \ !i ->
                convertA (crossArray tangleA `unsafeAt` (4 * nA + (lpA + legsToGlue + i) `mod` lA))
                    >>= unsafeWrite cr (4 * newC + i)
            forM_ [0 .. lB - legsToGlue - 1] $ \ !i ->
                convertB (crossArray tangleB `unsafeAt` (4 * nB + (lpB + 1 + i) `mod` lB))
                    >>= unsafeWrite cr (4 * newC + lA - legsToGlue + i) 
            unsafeFreeze cr

        st <- do
            st <- newArray_ (0, newC - 1) :: ST s (STArray s Int a)
            forM_ [0 .. nA - 1] $ \ !i ->
                unsafeWrite st i $ stateArray tangleA `unsafeAt` i
            forM_ [0 .. nB - 1] $ \ !i ->
                unsafeWrite st (i + nA) $ stateArray tangleB `unsafeAt` i
            unsafeFreeze st

        extraLoops <- do
            let markA a = do
                    let ai = 4 * nA + (lpA + a) `mod` lA
                        bi = crossArray tangleA `unsafeAt` ai
                        b = (bi - 4 * nA - lpA) `mod` lA
                    v <- readArray visited b
                    unless v $ unsafeWrite visited b True >> markB b

                markB a = do
                    let ai = 4 * nB + (lpB - a) `mod` lB
                        bi = crossArray tangleB `unsafeAt` ai
                        b = (lpB - (bi - 4 * nB)) `mod` lB
                    v <- readArray visited b
                    unless v $ unsafeWrite visited b True >> markA b

            foldM (\ !s !i -> do
                    v <- readArray visited i
                    if v
                        then return $! s
                        else markA i >> (return $! s + 1)
                ) 0 [0 .. legsToGlue - 1]

        return Tangle
            { loopsCount = numberOfFreeLoops tangleA + numberOfFreeLoops tangleB + extraLoops
            , crossCount = newC
            , crossArray = cr
            , stateArray = st
            , legsCount  = newL
            }

    glueToBorder leg legsToGlue !crossingToGlue = runST $ do
        unless (isLeg leg) $ fail $
            printf "glueToBorder: leg expected, but %s received" (show leg)

        when (legsToGlue < 0 || legsToGlue > 4) $ fail $
            printf "glueToBorder: legsToGlue must be in [0 .. 4], but %i found" legsToGlue

        let tangle = dartTangle leg
            oldL = numberOfLegs tangle
        when (oldL < legsToGlue) $ fail $
            printf "glueToBorder: not enough legs to glue (l = %i, legsToGlue = %i)" oldL legsToGlue

        let oldC = numberOfCrossings tangle
            newC = oldC + 1
            newL = oldL + 4 - 2 * legsToGlue
            lp = legPlace leg

        cr <- do
            cr <- newArray_ (0, 4 * newC + newL - 1) :: ST s (STUArray s Int Int)

            let {-# INLINE copyModified #-}
                copyModified !index !index' =
                    let y | x < 4 * oldC            = x
                          | ml < oldL - legsToGlue  = 4 * newC + 4 - legsToGlue + ml
                          | otherwise               = 4 * newC - 5 + oldL - ml
                          where
                              x = crossArray tangle `unsafeAt` index'
                              ml = (x - 4 * oldC - lp - 1) `mod` oldL
                    in unsafeWrite cr index y

            forM_ [0 .. 4 * oldC - 1] $ \ !i ->
                copyModified i i

            forM_ [0 .. legsToGlue - 1] $ \ !i ->
                copyModified (4 * (newC - 1) + i) (4 * oldC + ((lp - i) `mod` oldL))

            forM_ [0 .. 3 - legsToGlue] $ \ !i ->
                let a = 4 * (newC - 1) + legsToGlue + i
                    b = 4 * newC + i
                in unsafeWrite cr a b >> unsafeWrite cr b a

            forM_ [0 .. oldL - 1 - legsToGlue] $ \ !i ->
                copyModified (4 * newC + i + 4 - legsToGlue) (4 * oldC + ((lp + 1 + i) `mod` oldL))

            unsafeFreeze cr

        st <- do
            st <- newArray_ (0, newC - 1) :: ST s (STArray s Int a)
            forM_ [0 .. oldC - 1] $ \ !i ->
                unsafeWrite st i $ stateArray tangle `unsafeAt` i
            unsafeWrite st (newC - 1) crossingToGlue
            unsafeFreeze st

        return $! flip nthCrossing newC Tangle
            { loopsCount = numberOfFreeLoops tangle
            , crossCount = newC
            , crossArray = cr
            , stateArray = st
            , legsCount  = newL
            }


zeroTangle :: (CrossingType ct) => Tangle ct
zeroTangle = Tangle
    { loopsCount = 0
    , crossCount = 0
    , crossArray = listArray (0, 3) [3, 2, 1, 0]
    , stateArray = listArray (0, -1) []
    , legsCount  = 4
    }


infinityTangle :: (CrossingType ct) => Tangle ct
infinityTangle = Tangle
    { loopsCount = 0
    , crossCount = 0
    , crossArray = listArray (0, 3) [1, 0, 3, 2]
    , stateArray = listArray (0, -1) []
    , legsCount  = 4
    }


lonerTangle :: (CrossingType ct) => CrossingState ct -> Tangle ct
lonerTangle !cr = Tangle
    { loopsCount = 0
    , crossCount = 1
    , crossArray = listArray (0, 7) [4, 5, 6, 7, 0, 1, 2, 3]
    , stateArray = listArray (0, 0) [cr]
    , legsCount  = 4
    }


produceShowDart ''Tangle ''Dart $ \ d -> [([| isLeg $d |], [| printf "(Leg %i)" $ legPlace $d |])]
produceShowCrossing ''Tangle ''Crossing

instance (CrossingType ct) => Show (Tangle ct) where
    show tangle =
        let border = printf "(Border [ %s ])" $ unwords $ map (show . opposite) $ allLegs tangle
        in printf "(Tangle (%i O) %s)"
            (numberOfFreeLoops tangle)
            (unwords $ border : map show (allCrossings tangle))


instance KnottedWithConnectivity Tangle where
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
