module Math.Topology.KnotTh.Invariants.Skein.SkeinM.ContractEdge
    ( tryGreedyContract
    , contractEdgeST
    ) where

import Data.Array.IArray ((!))
import Data.Array.MArray (readArray)
import Control.Monad.ST (ST)
import Control.Monad (forM_, when)
import Text.Printf
import Math.Topology.KnotTh.Invariants.Skein.Relation
import Math.Topology.KnotTh.Invariants.Skein.SkeinM.State


tryGreedyContract :: (SkeinRelation r a) => SkeinState s r a -> Int -> ST s Bool
tryGreedyContract s v = do
    vd <- vertexDegreeST s v
    let tryContract !p
            | p >= vd    = return False
            | otherwise  = do
                (u, q) <- neighbourST s (v, p)
                if u == 0 then tryContract (p + 1) else do
                    ud <- vertexDegreeST s u
                    let go !w !i !j
                            | w >= vd    = return $! vd
                            | otherwise  = do
                                nb <- neighbourST s (v, i)
                                if nb == (u, j)
                                    then go (w + 1) ((i + 1) `mod` vd) ((j - 1) `mod` ud)
                                    else return $! w

                    w <- go 0 p q
                    if ud + vd - 2 * w <= max vd ud
                        then contractEdgeST s (v, p) >> return True
                        else tryContract (p + w)
    tryContract 0


contractEdgeST :: (SkeinRelation r a) => SkeinState s r a -> (Int, Int) -> ST s ()
contractEdgeST s (!v, !p) = do
    (!u, !q) <- neighbourST s (v, p)
    when (v == 0 || u == 0 || v == u) $ fail $
        printf "contract: can not contract (%i, %i) <-> (%i, %i)" v p u q

    degreeV <- vertexDegreeST s v
    degreeU <- vertexDegreeST s u
    enqueueST s =<<
        if degreeV >= degreeU
            then contract s (v, p) (u, q)
            else contract s (u, q) (v, p)


contract :: (SkeinRelation r a) => SkeinState s r a -> (Int, Int) -> (Int, Int) -> ST s Int
contract s (!v, !p) (!u, !q) = do
    degreeV <- vertexDegreeST s v
    degreeU <- vertexDegreeST s u

    sumV <- getStateSumST s v
    sumU <- getStateSumST s u
    let (substV, substU, resultSum) = connect (relation s) (p, sumV) (q, sumU)
    setStateSumST s v resultSum

    do
        prevV <- resizeAdjListST s v $ degreeV + degreeU - 2
        prevU <- getAdjListST s u

        forM_ [0 .. degreeV - 1] $ \ !i -> when (i /= p) $ do
            (w, k) <- readArray prevV i
            connectST s (v, substV ! i) $ case () of
                _ | w == v    -> (v, substV ! k)
                  | w == u    -> (v, substU ! k)
                  | otherwise -> (w, k)

        forM_ [0 .. degreeU - 1] $ \ !i -> when (i /= q) $ do
            (w, k) <- readArray prevU i
            connectST s (v, substU ! i) $ case () of
                _ | w == v    -> (v, substV ! k)
                  | w == u    -> (v, substU ! k)
                  | otherwise -> (w, k)

    killVertexST s u
    return $! v
