{-# LANGUAGE TypeFamilies, RankNTypes #-}
module Math.Topology.KnotTh.Moves.ModifyDSL
    ( ModifyDSL(..)
    , greedy
    ) where

import Control.Monad (when)
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Crossings.Diagram


class (Knotted k) => ModifyDSL k where
    data ModifyM k :: * -> * -> * -> *

    modifyKnot     :: (Show a) => k a -> (forall s. (Monad (ModifyM k a s)) => ModifyM k a s ()) -> k a
    aliveCrossings :: ModifyM k a s [Vertex k a]

    emitLoopsC  :: Int -> ModifyM k a s ()
    oppositeC   :: Dart k a -> ModifyM k a s (Dart k a)
    passOverC   :: Dart k DiagramCrossing -> ModifyM k DiagramCrossing s Bool
    maskC       :: [Vertex k a] -> ModifyM k a s ()
    isMaskedC   :: Vertex k a -> ModifyM k a s Bool
    modifyC     :: (Show a) => Bool -> (a -> a) -> [Vertex k a] -> ModifyM k a s ()
    connectC    :: [(Dart k a, Dart k a)] -> ModifyM k a s ()
    substituteC :: [(Dart k a, Dart k a)] -> ModifyM k a s ()


greedy :: (ModifyDSL k, Monad (ModifyM k a s)) => [Dart k a -> ModifyM k a s Bool] -> ModifyM k a s ()
greedy reductionsList = iteration
    where
        iteration = do
            crs <- aliveCrossings
            changed <- anyM (\ d -> anyM ($ d) reductionsList) $ crs >>= outcomingDarts
            when changed iteration

        anyM :: (Monad m) => (a -> m Bool) -> [a] -> m Bool
        anyM _ [] = return False
        anyM f (cur : rest) = do
            res <- f cur
            if res
                then return True
                else anyM f rest
