module Math.Topology.KnotTh.Moves.PatternMatching
    ( patternMatching
    , flypePattern
    , passPattern
    ) where

import Control.Monad (MonadPlus(..), guard)
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Moves.AdHocOfTangle.Move


data PatternS a = PatternS (Tangle a) [Vertex Tangle a]

newtype PatternM a x = PatternM { runPatternMatching :: PatternS a -> [(PatternS a, x)] }


instance Functor (PatternM a) where
    fmap f x = PatternM (map (fmap f) . runPatternMatching x)


instance Monad (PatternM a) where
    return x = PatternM (\ s -> [(s, x)])

    (>>=) val act = PatternM (concatMap (\ (s', x) -> runPatternMatching (act x) s') . runPatternMatching val)


instance MonadPlus (PatternM a) where
    mzero = PatternM (const [])

    mplus a b = PatternM (\ s -> runPatternMatching a s ++ runPatternMatching b s)


subTangleP :: Int -> PatternM a [Dart Tangle a]
subTangleP _ = undefined


crossingP :: PatternM a [Dart Tangle a]
crossingP =
    PatternM $ \ (PatternS tangle cs) ->
        let try res _ [] = res
            try res skipped (cur : rest) =
                let next = PatternS tangle (skipped ++ rest)
                    sh i = let od = outcomingDarts cur
                           in drop i od ++ take i od
                in try ((next, sh 0) : (next, sh 1) : (next, sh 2) : (next, sh 3) : res) (cur : skipped) rest
        in try [] [] cs


connectionP :: [(Dart Tangle a, Dart Tangle a)] -> PatternM a ()
connectionP = mapM_ (\ (a, b) -> guard (opposite a == b))


getTangleP :: PatternM a (Tangle a)
getTangleP = PatternM (\ s@(PatternS t _) -> [(s, t)])


flypePattern :: PatternM DiagramCrossing ()
flypePattern = do
    [c, d, _, _] <- subTangleP 4
    [a, b, _, _] <- crossingP
    connectionP [(b, c), (a, d)]


passPattern :: PatternM DiagramCrossing TangleDiagram
passPattern = do
    [a0, _, _, a3] <- crossingP
    [b0, b1, _, _] <- crossingP
    connectionP [(a0, b0)]
    guard $ alternatingDefect a0 > 0

    [c0, c1, c2, c3] <- crossingP
    connectionP [(c0, b1), (c1, a3)]

    tangle <- getTangleP

    let pass incoming outcoming = move tangle $ do
            substituteC $ map (\ d -> (d, threadContinuation $ opposite d)) incoming ++ zip (map opposite incoming) outcoming
            connectC $ zip outcoming $ map (threadContinuation . opposite) incoming

    return $ pass [c1, c0] [c2, c3]


patternMatching :: PatternM a (Tangle a) -> Tangle a -> [Tangle a]
patternMatching pat tangle =
    map snd $ runPatternMatching pat $ PatternS tangle (allVertices tangle)
