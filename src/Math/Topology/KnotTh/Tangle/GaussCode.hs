{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Tangle.GaussCode
    ( toDTCode
    , fromDTCode
    , toGaussCode
    , fromGaussCode
    ) where

import Control.Monad (foldM_, forM, forM_, liftM2, unless, when)
import qualified Control.Monad.ST as ST
import qualified Data.Array.ST as STArray
import qualified Data.Array.Unboxed as A
import Data.List (mapAccumL, findIndices)
import qualified Data.Map as M
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Text.Printf
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.Crossings.Diagram
import Math.Topology.KnotTh.Knotted.Threads
import Math.Topology.KnotTh.Tangle.TangleDef


toDTCode :: Tangle0 DiagramCrossing -> [[Int]]
toDTCode _ = error "toDTCode: not implemented"


fromDTCode :: [[Int]] -> Tangle0 DiagramCrossing
fromDTCode code =
    let common = concat code
        sz = length common

        a :: A.UArray Int Int
        a = A.array (1, 2 * sz) $ do
            (i, x) <- [0 ..] `zip` common
            when (odd x) $ error $
                printf "fromDTCode: at %s: all numbers must be even, but %i is not" (show code) x
            when (abs x < 1 || abs x > 2 * sz) $ error $
                printf "fromDTCode: at %s: absolute value of %i is out of bounds (1, %i)" (show code) x (2 * sz)
            [(abs x, x), (2 * i + 1, -x)]

    in fromGaussCode $ snd $ mapAccumL
        (\ !i !thread ->
            let n = 2 * length thread
            in (i + n, map ((a A.!) . (i +)) [0 .. n - 1])
        ) 1 code


toGaussCode :: Tangle0 DiagramCrossing -> [[Int]]
toGaussCode link =
    let encode d | isPassingOver d  = idx
                 | otherwise        = -idx
            where idx = vertexIndex (beginVertex d)
    in map (map (encode . snd)) $ allThreads link


fromGaussCode :: [[Int]] -> Tangle0 DiagramCrossing
fromGaussCode =
    let splice i = go
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

        decode (n, threads) = implode (length $ filter null threads, incidence)
            where
                chords = foldl (flip splice) (filter (not . null) threads) [1 .. n]

                color = STArray.runSTUArray $ do
                    vis <- STArray.newArray (1, n) False :: ST.ST s (STArray.STUArray s Int Bool)
                    col <- STArray.newArray_ (1, n) :: ST.ST s (STArray.STUArray s Int Bool)

                    let edge [] _ _ = False
                        edge (h : t) i j =
                            case (findIndices ((== i) . abs) h, findIndices ((== j) . abs) h) of
                                ([], [])         -> edge t i j
                                ([a, b], [c, d]) -> (a < c && b > c && b < d) || (a > c && a < d && b > d)
                                _                -> False

                    let dfs c i = do
                            v <- STArray.readArray vis i
                            if v
                                then do
                                    c' <- STArray.readArray col i
                                    when (c' /= c) $ fail "fromGaussCode: gauss code is not planar"
                                else do
                                    STArray.writeArray vis i True
                                    STArray.writeArray col i c
                                    forM_ [1 .. n] $ \ j ->
                                        when (edge chords i j) $ dfs (not c) j

                    forM_ [1 .. n] $ \ !i -> do
                        v <- STArray.readArray vis i
                        unless v $ dfs False i

                    return $! col

                incidence = ST.runST $ do
                    conn <- STArray.newArray_ ((1, 0), (n, 3)) :: ST.ST s (STArray.STArray s (Int, Int) (Int, Int))
                    state <- STArray.newArray_ (1, n) :: ST.ST s (STArray.STArray s Int DiagramCrossing)

                    let connect a b = STArray.writeArray conn a b >> STArray.writeArray conn b a

                    let connection second out i =
                            let j | not second && not out        = 0
                                  | second     && not out        = 2
                                  | (color A.! abs i) == second  = 3
                                  | otherwise                    = 1
                            in (abs i, j)

                    vis <- STArray.newArray (1, n) False :: ST.ST s (STArray.STUArray s Int Bool)
                    forM_ chords $ \ chord ->
                        foldM_ (\ prev i -> do
                            second <- STArray.readArray vis $ abs i
                            STArray.writeArray vis (abs i) True
                            connect prev (connection second False i)

                            let crossing = overCrossingIf (i > 0)

                            if second
                                then STArray.readArray state (abs i) >>= flip when (fail "gauss code internal error") . (/= crossing)
                                else STArray.writeArray state (abs i) crossing

                            return $! connection second True i
                            ) (connection True True $ last chord) chord

                    forM [1 .. n] $ \ i -> liftM2 (,)
                        (forM [0 .. 3] $ \ j -> STArray.readArray conn (i, j))
                        (STArray.readArray state i)

        simplifyGaussCode code = ST.runST $ do
            let n = sum $ flip map code $ \ x ->
                    let n2 = length x
                    in if even n2
                        then n2 `div` 2
                        else error "fromGaussCode: lengths must be even"

            index <- do
                free <- newSTRef 1
                indices <- newSTRef M.empty
                return $ \ !x -> do
                    m <- readSTRef indices
                    if M.member x m
                        then return $! m M.! x
                        else do
                            y <- readSTRef free
                            when (y > n) $ fail "fromGaussCode: too many different values in gauss code"
                            writeSTRef free $! y + 1
                            writeSTRef indices $! M.insert x y m
                            return $! y

            visitedP <- STArray.newArray (1, n) False :: ST.ST s (STArray.STUArray s Int Bool)
            visitedN <- STArray.newArray (1, n) False :: ST.ST s (STArray.STUArray s Int Bool)

            let pickV x | x > 0      = visitedP
                        | x < 0      = visitedN
                        | otherwise  = error "fromGaussCode: zero presented"

            simplified <-
                forM code $ mapM $ \ !raw -> do
                    i <- index $ abs raw
                    STArray.readArray (pickV raw) i >>= \ v ->
                        when v $ fail $ "fromGaussCode: duplication on " ++ show raw
                    STArray.writeArray (pickV raw) i True
                    return $! i * signum raw

            return (n, simplified)

    in decode . simplifyGaussCode
