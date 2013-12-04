module Math.Topology.KnotTh.Moves.PatternMatching
    ( patternMatching
    , flypePattern
    ) where

import Math.Topology.KnotTh.Tangle


data PatternM a x = PatternM


instance Monad (PatternM a) where
    return _ = undefined

    (>>=) _ _ = undefined


data DartP a = DartP


subTangleP :: Int -> PatternM a ((), [DartP a])
subTangleP _ = undefined

crossingP :: PatternM a ((), [DartP a])
crossingP = undefined


connectionP :: [(DartP a, DartP a)] -> PatternM a ()
connectionP _ = undefined


flypePattern :: PatternM DiagramCrossing ()
flypePattern = do
    (_, [c, d, _, _]) <- subTangleP 4
    (_, [a, b, _, _]) <- crossingP
    connectionP [(b, c), (a, d)]


patternMatching :: PatternM a () -> Tangle a -> [Tangle a]
patternMatching _ _ = undefined
