{-# LANGUAGE TemplateHaskell #-}
module Math.KnotTh.Tangle.Def.Tangle
    ( module Math.KnotTh.Knotted
    , Dart
    , Crossing
    , Tangle
    , crossingTangle
    , dartTangle
    , numberOfLegs
    , isLeg
    , legPlace
    , nthLeg
    , allLegs
    , glueToBorder
    , changeNumberOfFreeLoops
    , implode
    , explode
    ) where

import Language.Haskell.TH
import Data.List (intercalate, nub, sort, foldl')
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Array.Base (unsafeAt, unsafeWrite, unsafeRead)
import Data.Array.ST (STArray, STUArray, newArray_)
import Data.Array.Unsafe (unsafeFreeze)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_, when)
import Text.Printf
import Math.KnotTh.Knotted.TH.Knotted
import Math.KnotTh.Knotted.TH.Show
import Math.KnotTh.Knotted


produceKnotted
    [d| data Tangle ct = Tangle { numberOfLegs :: {-# UNPACK #-} !Int } |] $
    let numberOfLegs = varE $ mkName "numberOfLegs"
        dart = conE $ mkName "Dart"
    in defaultKnotted
        { implodeExplodeSettings = Nothing

        , modifyNumberOfEdges = Just $ \ t _ -> [|
            2 * (numberOfCrossings $(t)) + ($(numberOfLegs) $(t) `div` 2)
            |]

        , modifyIsDart = Just $ \ (t, i) -> [|
            $(i) < 4 * numberOfCrossings $(t)
            |]

        , modifyNextCCW = Just $ \ (t, d) e -> [|
            let n = (4 :: Int) * numberOfCrossings $(t)
            in if $(d) >= n
                then $(dart) $(t) $! n + ($(d) - n + 1) `mod` ($(numberOfLegs) $(t))
                else $(e)
            |]

        , modifyNextCW = Just $ \ (t, d) e -> [|
            let n = (4 :: Int) * numberOfCrossings $(t)
            in if $(d) >= n
                then $(dart) $(t) $! n + ($(d) - n - 1) `mod` ($(numberOfLegs) $(t))
                else $(e)
            |]

        , modifyDartPlace = Just $ \ (t, d) e -> [|
            let n = (4 :: Int) * numberOfCrossings $(t)
            in if $(d) >= n
                then error $ printf "dartPlace: taken from %i-th leg" ($(d) - n)
                else $(e)
            |]

        , modifyIncidentCrossing = Just $ \ (t, d) e -> [|
            let n = (4 :: Int) * numberOfCrossings $(t)
            in if $(d) >= n
                then error $ printf "incidentCrossing: taken from %i-th leg" ($(d) - n)
                else $(e)
            |]

        , modifyFoldMIncidentDartsFrom = Just $ \ (d, _) e -> [|
            if $(varE $ mkName "isLeg") $(d)
                then error $ printf "foldMIncidentDartsFrom: taken from leg %i" ($(varE $ mkName "legPlace") $(d))
                else $(e)
            |]
        }


{-# INLINE isLeg #-}
isLeg :: Dart ct -> Bool
isLeg = not . isDart


{-# INLINE legPlace #-}
legPlace :: Dart ct -> Int
legPlace d@(Dart t i)
    | isDart d   = error $ printf "legPlace: taken from non-leg %s" $ show d
    | otherwise  = i - 4 * numberOfCrossings t


{-# INLINE nthLeg #-}
nthLeg :: Tangle ct -> Int -> Dart ct
nthLeg t i =
    let n = 4 * numberOfCrossings t
        l = numberOfLegs t
    in Dart t $! n + i `mod` l


{-# INLINE allLegs #-}
allLegs :: Tangle ct -> [Dart ct]
allLegs t =
    let n = 4 * numberOfCrossings t
        l = numberOfLegs t
    in map (Dart t) [n .. n + l - 1]


--       edgesToGlue = 1                 edgesToGlue = 2                 edgesToGlue = 3
-- ........|                       ........|                       ........|
-- (leg+1)-|---------------3       (leg+1)-|---------------2       (leg+1)-|---------------1
--         |  +=========+                  |  +=========+                  |  +=========+
--  (leg)--|--|-0-\ /-3-|--2        (leg)--|--|-0-\ /-3-|--1        (leg)--|--|-0-\ /-3-|--0
-- ........|  |    *    |                  |  |    *    |                  |  |    *    |
-- ........|  |   / \-2-|--1       (leg-1)-|--|-1-/ \-2-|--0       (leg-1)-|--|-1-/ \   |
-- ........|  |  1      |          ........|  +=========+                  |  |      2  |
-- ........|  |   \-----|--0       ........|                       (leg-2)-|--|-----/   |
-- ........|  +=========+          ........|                       ........|  +=========+
glueToBorder :: (CrossingType ct) => Dart ct -> Int -> CrossingState ct -> Crossing ct
glueToBorder leg legsToGlue crossingToGlue
    | not (isLeg leg)                   = error $ printf "glueToBorder: leg expected, %s received" (show leg)
    | legsToGlue < 1 || legsToGlue > 3  = error $ printf "glueToBorder: legsToGlue must be 1, 2 or 3, but %i found" legsToGlue
    | otherwise                         = runST $ do
        let tangle = dartTangle leg
        let oldL = numberOfLegs tangle
        when (oldL <= legsToGlue) $ fail $
            printf "glueToBorder: not enough legs to glue (l = %i, legsToGlue = %i)" oldL legsToGlue

        let oldC = numberOfCrossings tangle
        let newC = oldC + 1
        let newL = oldL + 4 - 2 * legsToGlue
        let lp = legPlace leg

        cr <- newArray_ (0, 4 * newC + newL - 1) :: ST s (STUArray s Int Int)

        let {-# INLINE copyModified #-}
            copyModified !index !index' = do
                let y | x < 4 * oldC            = x
                      | ml < oldL - legsToGlue  = 4 * newC + 4 - legsToGlue + ml
                      | otherwise               = 4 * newC - 5 + oldL - ml
                      where
                          x = crossArray tangle `unsafeAt` index' 
                          ml = (x - 4 * oldC - lp - 1) `mod` oldL
                unsafeWrite cr index y

        forM_ [0 .. 4 * oldC - 1] $ \ !i ->
            copyModified i i

        forM_ [0 .. legsToGlue - 1] $ \ !i ->
            copyModified (4 * (newC - 1) + i) (4 * oldC + ((lp - i) `mod` oldL))

        forM_ [0 .. 3 - legsToGlue] $ \ !i -> do
            let a = 4 * (newC - 1) + legsToGlue + i
            let b = 4 * newC + i
            unsafeWrite cr a b
            unsafeWrite cr b a

        forM_ [0 .. oldL - 1 - legsToGlue] $ \ !i ->
            copyModified (4 * newC + i + 4 - legsToGlue) (4 * oldC + ((lp + 1 + i) `mod` oldL))

        st <- newArray_ (0, newC - 1) :: ST s (STArray s Int a)
        forM_ [0 .. oldC - 1] $ \ !i ->
            unsafeWrite st i $! stateArray tangle `unsafeAt` i
        unsafeWrite st (newC - 1) crossingToGlue

        cr' <- unsafeFreeze cr
        st' <- unsafeFreeze st
        let result = Tangle
                { loopsCount   = numberOfFreeLoops tangle
                , crossCount   = newC
                , crossArray   = cr'
                , stateArray   = st' 
                , numberOfLegs = newL
                }

        return $! nthCrossing result newC


implode :: (CrossingType ct) => (Int, [(Int, Int)], [([(Int, Int)], CrossingState ct)]) -> Tangle ct
implode arg@(!loops, !border, !list) = runST $ do
    when (loops < 0) $ fail $
        printf "implode: number of free loops is negative (%i)" loops

    let n = length list
    let l = length border
    when (odd l) $ fail $
        printf "implode: number of legs must be even (%i) in %s" l (show arg)

    let {-# INLINE testPair #-}
        testPair c p = case c of
            0 | p < 0 || p >= l -> fail $ printf "implode: leg index %i is out of bound" p
              | otherwise       -> return ()
            _ | c < 0 || c > n  -> fail $ printf "implode: crossing index %i is out of bounds (1, %i)" c n
              | p < 0 || p > 3  -> fail $ printf "implode: place %i index is out of bound" p
              | otherwise       -> return ()


    cr <- newArray_ (0, 4 * n + l - 1) :: ST s (STUArray s Int Int)
    st <- newArray_ (0, n - 1) :: ST s (STArray s Int a)

    forM_ (zip list [0 ..]) $ \ ((!ns, !state), !i) -> do
        unsafeWrite st i state
        when (length ns /= 4) $ fail $
            printf "implode: there must be 4 neighbours for every crossing, but got %i at %i" (length ns) (i + 1)

        forM_ (zip ns [0 ..]) $ \ ((!c, !p), !j) -> do
            testPair c p

            let a = 4 * i + j
            let b | c == 0     = 4 * n + p
                  | otherwise  = 4 * (c - 1) + p

            when (a == b) $ fail $
                printf "implode: dart (%i, %i) connected to itself" (i + 1) j

            unsafeWrite cr a b
            when (b < a) $ do
                x <- unsafeRead cr b
                when (x /= a) $ fail $
                    printf "implode: inconsistent data at dart (%i, %i) in %s" (i + 1) j (show arg)


    forM_ (zip border [0 ..]) $ \ ((!c, !p), !i) -> do
        testPair c p

        let a = 4 * n + i
        let b | c == 0     = 4 * n + p
              | otherwise  = 4 * (c - 1) + p

        when (a == b) $ fail $
            printf "implode: leg %i connected to itself in %s" i (show arg)

        unsafeWrite cr a b
        when (b < a) $ do
            x <- unsafeRead cr b
            when (x /= a) $ fail $
                printf "implode: inconsistent data at leg %i in %s" i (show arg)


    cr' <- unsafeFreeze cr
    st' <- unsafeFreeze st
    return $! Tangle
        { loopsCount   = loops
        , crossCount   = n
        , crossArray   = cr'
        , stateArray   = st' 
        , numberOfLegs = l
        }


explode :: (CrossingType ct) => Tangle ct -> (Int, [(Int, Int)], [([(Int, Int)], CrossingState ct)])
explode tangle =
    let crToList c = (map (toPair . opposite) $ incidentDarts c, crossingState c)
    in (numberOfFreeLoops tangle, map (toPair . opposite) $ allLegs tangle, map crToList $ allCrossings tangle)


instance Show (Dart ct) where
    show d
        | isLeg d    = printf "(Leg %i)" $ legPlace d
        | otherwise  =
            let (c, p) = toPair d
            in printf "(Dart %i %i)" c p


produceShowCrossing ''Crossing


instance (CrossingType ct) => Show (Tangle ct) where
    show tangle =
        let border = printf "(Border [ %s ])" $
                intercalate " " $ map (show . opposite) $ allLegs tangle
        in printf "(Tangle (%i O) %s)"
            (numberOfFreeLoops tangle)
            (intercalate " " $ border : map show (allCrossings tangle))


instance KnottedWithToPair Tangle Crossing Dart where
    toPair d
        | isLeg d    =
            let p = legPlace d
            in p `seq` (0, p)
        | otherwise  =
            let c = crossingIndex $ incidentCrossing d
                p = dartPlace d
            in c `seq` p `seq` (c, p)


instance KnottedWithConnectivity Tangle Crossing Dart where
    isConnected tangle
        | numberOfEdges tangle == 0 && numberOfFreeLoops tangle <= 1  = True
        | numberOfFreeLoops tangle /= 0                               = False
        | otherwise                                                   = all (\ (a, b) -> Set.member a con && Set.member b con) edges
        where
            edges = allEdges tangle
            con = dfs (Set.empty) $ fst $ head edges
            dfs vis c
                | Set.member c vis  = vis
                | otherwise         = foldl' dfs (Set.insert c vis) neigh
                where
                    neigh
                        | isLeg c    = [opposite c]
                        | otherwise  = [opposite c, nextCCW c, nextCW c]

    isPrime tangle = connections == nub connections
        where
            idm =    let faces = directedPathsDecomposition (nextCW, nextCCW)
                in Map.fromList $ concatMap (\ (face, i) -> zip face $ repeat i) $ zip faces [(0 :: Int) ..]

            connections = sort $ map getPair $ allEdges tangle
                where
                    getPair (da, db) = (min a b, max a b)
                        where
                            a = idm Map.! da
                            b = idm Map.! db

            directedPathsDecomposition continue = fst $ foldl' processDart ([], Set.empty) $ allHalfEdges tangle
                where
                    processDart (paths, s) d
                        | Set.member d s  = (paths, s)
                        | otherwise       = (path : paths, nextS)
                        where
                            path = containingDirectedPath continue d
                            nextS = foldl' (\ curs a -> Set.insert a curs) s path

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
