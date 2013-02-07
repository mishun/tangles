module Math.KnotTh.Invariants.Skein.SkeinM.RelaxVertex
    ( tryRelaxVertex
    ) where

import Data.Array.Base ((!), readArray)
import Control.Monad.ST (ST)
import Control.Monad (forM_, when)
import Text.Printf
import Math.KnotTh.Invariants.Skein.Relation
import Math.KnotTh.Invariants.Skein.SkeinM.State
import Math.KnotTh.Invariants.Skein.StateSum


tryRelaxVertex :: (SkeinRelation r a) => SkeinState s r a -> Int -> ST s Bool
tryRelaxVertex s v = do
    degree <- vertexDegreeST s v
    case degree of
        0 -> do
            dissolveVertexST s v
            return True

        2 -> do
            a@(av, _) <- neighbourST s (v, 0)
            b@(bv, _) <- neighbourST s (v, 1)
            dissolveVertexST s v
            if a == (v, 1)
                then appendMultipleST s $ circleFactor $ relation s
                else do
                    connectST s a b
                    when (av /= 0) $ enqueueST s av
                    when (bv /= 0) $ enqueueST s bv
            return True

        _ -> do
            let findLoop [] = return False
                findLoop (i : t) = do
                    (u, j) <- neighbourST s (v, i)
                    if u /= v || j /= (i + 1) `mod` degree
                        then findLoop t
                        else do
                            contractLoopST s (v, i)
                            enqueueST s v
                            return True
            findLoop [0 .. degree - 1]


dissolveVertexST :: (Num a) => SkeinState s r a -> Int -> ST s ()
dissolveVertexST s v = do
    stateSum <- getStateSumST s v
    appendMultipleST s $ takeAsConst stateSum
    killVertexST s v


contractLoopST :: (SkeinRelation r a) => SkeinState s r a -> (Int, Int) -> ST s ()
contractLoopST s (!v, !p) = do
    degree <- vertexDegreeST s v
    (!v', !p') <- neighbourST s (v, p)
    when (v == 0 || v' /= v || p' /= (p + 1) `mod` degree) $ fail $
        printf "contractLoopST: not loop (%i, %i) <-> (%i, %i) / %i" v p v' p' degree

    preSum <- getStateSumST s v
    let (subst, postSum) = glueHandle (relation s) degree p preSum
    setStateSumST s v postSum

    prev <- resizeAdjListST s v $ degree - 2
    forM_ [0 .. degree - 1] $ \ !i -> when (i /= p' && i /= p) $ do
        (u, j) <- readArray prev i
        connectST s (v, subst ! i) $ if u /= v then (u, j) else (v, subst ! j)
