module Math.Topology.KnotTh.Link.Definition.GaussCode
    ( toDTCode
    , fromDTCode
    , toGaussCode
    , fromGaussCode
    ) where

import Data.List (mapAccumL, findIndices)
import qualified Data.Map as M
import Data.Array.IArray ((!), array)
import Data.Array.MArray (newArray, newArray_, readArray, writeArray)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray, STUArray, runSTUArray)
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM, forM_, when, unless, foldM_, liftM2)
import Text.Printf
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Crossings.Diagram
import Math.Topology.KnotTh.Link.Definition.Link


toDTCode :: LinkDiagram -> [[Int]]
toDTCode _ = error "not implemented"


fromDTCode :: [[Int]] -> LinkDiagram
fromDTCode code =
    let common = concat code
        sz = length common

        a :: UArray Int Int
        a = array (1, 2 * sz) $ do
            (i, x) <- zip [0 ..] common
            when (odd x) $ error $
                printf "fromDTCode: at %s: all numbers must be even, but %i is not" (show code) x
            when (abs x < 1 || abs x > 2 * sz) $ error $
                printf "fromDTCode: at %s: absolute value of %i is out of bounds (1, %i)" (show code) x (2 * sz)
            [(abs x, x), (2 * i + 1, -x)]

    in fromGaussCode $ snd $ mapAccumL
        (\ !i !thread ->
            let n = 2 * length thread
            in (i + n, map ((a !) . (i +)) [0 .. n - 1])
        ) 1 code


toGaussCode :: LinkDiagram -> [[Int]]
toGaussCode link =
    flip map (allThreads link) $ map $ \ (_, d) ->
        (vertexIndex $ beginVertex d) * (if passOver d then 1 else -1)


fromGaussCode :: [[Int]] -> LinkDiagram
fromGaussCode = decode . simplifyGaussCode


splice :: Int -> [[Int]] -> [[Int]]
splice i = go
    where
        go [] = error "internal error: not found"
        go (thread : threads) =
            case span ((/= i) . abs) thread of
                (_, [])     -> thread : go threads
                (pref, suf) ->
                    case span ((/= i) . abs) $ tail suf of
                        (_, [])   ->
                            let (x, r) = match threads
                            in (pref ++ [head suf] ++ x ++ tail suf) : r
                        (int, tl) ->
                            let r = reverse $ map (\ j -> if abs j < abs i then -j else j) int
                            in (pref ++ [head suf] ++ r ++ [-head tl] ++ tail tl) : threads

        match [] = error "internal error: not found"
        match (thread : threads) =
            case span ((/= i) . abs) thread of
                (_, [])     ->
                    let (a, b) = match threads
                    in (a, thread : b)
                (pref, suf) -> (tail suf ++ pref ++ [head suf], threads)


decode :: (Int, [[Int]]) -> LinkDiagram
decode (n, threads) = implode (length $ filter null threads, incidence)
    where
        chords = foldl (\ l i -> splice i l) (filter (not . null) threads) [1 .. n]

        color = runSTUArray $ do
            vis <- newArray (1, n) False :: ST s (STUArray s Int Bool)
            col <- newArray_ (1, n) :: ST s (STUArray s Int Bool)

            let edge [] _ _ = False
                edge (h : t) i j =
                    case (findIndices ((== i) . abs) h, findIndices ((== j) . abs) h) of
                        ([], [])         -> edge t i j
                        ([a, b], [c, d]) -> (a < c && b > c && b < d) || (a > c && a < d && b > d)
                        _                -> False

            let dfs c i = do
                    v <- readArray vis i
                    if v
                        then do
                            c' <- readArray col i
                            when (c' /= c) $ fail "fromGaussCode: gauss code is not planar"
                        else do
                            writeArray vis i True
                            writeArray col i c
                            forM_ [1 .. n] $ \ j ->
                                when (edge chords i j) $ dfs (not c) j

            forM_ [1 .. n] $ \ !i -> do
                v <- readArray vis i
                unless v $ dfs False i

            return $! col

        incidence = runST $ do
            conn <- newArray_ ((1, 0), (n, 3)) :: ST s (STArray s (Int, Int) (Int, Int))
            state <- newArray_ (1, n) :: ST s (STArray s Int DiagramCrossing)

            let connect a b = writeArray conn a b >> writeArray conn b a

            let connection second out i =
                    let j | not second && not out      = 0
                          | second     && not out      = 2
                          | (color ! abs i) == second  = 1
                          | otherwise                  = 3
                    in (abs i, j)

            vis <- newArray (1, n) False :: ST s (STUArray s Int Bool)
            forM_ chords $ \ chord -> do
                foldM_ (\ prev i -> do
                    second <- readArray vis $ abs i
                    writeArray vis (abs i) True
                    connect prev (connection second False i)

                    let crossing
                            | i > 0      = overCrossing
                            | otherwise  = underCrossing

                    if second
                        then readArray state (abs i) >>= flip when (fail "gauss code internal error") . (/= crossing)
                        else writeArray state (abs i) crossing

                    return $! connection second True i
                    ) (connection True True $ last chord) chord

            forM [1 .. n] $ \ i -> liftM2 (,)
                (forM [0 .. 3] $ \ j -> readArray conn (i, j))
                (readArray state i)


simplifyGaussCode :: [[Int]] -> (Int, [[Int]])
simplifyGaussCode code = runST $ do
    let n = sum $ flip map code $ \ x ->
            let n2 = length x
            in if even n2
                then n2 `div` 2
                else error "fromGaussCode: lengths must be even"

    index <- do
        free <- newSTRef 1
        indices <- newSTRef $ M.empty
        return $! \ !x -> do
            m <- readSTRef indices
            if M.member x m
                then return $! m M.! x
                else do
                    y <- readSTRef free
                    when (y > n) $ fail "fromGaussCode: too many different values in gauss code"
                    writeSTRef free $! y + 1
                    writeSTRef indices $! M.insert x y m
                    return $! y

    visitedP <- newArray (1, n) False :: ST s (STUArray s Int Bool)
    visitedN <- newArray (1, n) False :: ST s (STUArray s Int Bool)

    let pickV x | x > 0      = visitedP
                | x < 0      = visitedN
                | otherwise  = error "fromGaussCode: zero presented"

    simplified <-
        forM code $ mapM $ \ !raw -> do
            i <- index $ abs raw
            readArray (pickV raw) i >>= \ v ->
                when v $ fail $ "fromGaussCode: duplication on " ++ show raw
            writeArray (pickV raw) i True
            return $! i * signum raw

    return $! (n, simplified)
