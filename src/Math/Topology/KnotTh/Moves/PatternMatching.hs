module Math.Topology.KnotTh.Moves.PatternMatching
    ( patternMatching
    , flypePattern
    , passPattern
    ) where

import Data.Maybe (mapMaybe)
import Data.List ((\\), subsequences)
import qualified Data.Set as S
import Data.Array.IArray ((!), (//), listArray)
import Data.Array.Unboxed (UArray)
import Control.Monad.State (execState, gets, modify)
import Control.Monad (MonadPlus(..), unless, guard)
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


subTangleP :: Int -> PatternM a ([Dart Tangle a], [Vertex Tangle a])
subTangleP legs =
    PatternM $ \ (PatternS tangle cs) -> do
        subList <- subsequences cs
        guard $ not $ null subList

        let sub :: UArray Int Bool
            sub = listArray (vertexIndicesRange tangle) (repeat False)
                    // map (\ d -> (vertexIndex d, True)) subList

        guard $
            let mask = execState (dfs $ head subList) S.empty
                dfs c = do
                    visited <- gets $ S.member c
                    unless visited $ do
                        modify $ S.insert c
                        mapM_ dfs $ filter ((sub !) . vertexIndex) $ mapMaybe endVertexM $ outcomingDarts c
            in all (`S.member` mask) subList

        let onBorder xy =
                let yx = opposite xy
                    x = beginVertex xy
                    y = beginVertex yx
                in isDart xy && (sub ! vertexIndex x) && (isLeg yx || not (sub ! vertexIndex y))

        let border = filter onBorder $ concatMap outcomingDarts cs
        guard $ legs == length border

        let borderCCW =
                let traverseNext = nextCW . opposite
                    restoreOutcoming out d | d == head border  = d : out
                                           | onBorder d        = restoreOutcoming (d : out) (opposite d)
                                           | otherwise         = restoreOutcoming out (traverseNext d)
                in restoreOutcoming [] (opposite $ head border)

        guard $ legs == length borderCCW

        i <- [0 .. legs - 1]
        return (PatternS tangle (cs \\ subList), (drop i borderCCW ++ take i borderCCW, subList))


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


flypePattern :: PatternM DiagramCrossing TangleDiagram
flypePattern = do
    [ab, ac, ae, ad] <- crossingP
    ([ca, ba, rp, sq], sub) <- subTangleP 4
    guard $ length sub > 1
    connectionP [(ac, ca), (ab, ba)]

    tangle <- getTangleP
    return $ move tangle $ do
        substituteC [(ba, ae), (ca, ad), (ab, rp), (ac, sq)]
        connectC [(rp, ae), (sq, ad)]
        modifyC True id sub


passPattern :: PatternM DiagramCrossing TangleDiagram
passPattern = do
    [a0, _, _, a3] <- crossingP
    [b0, b1, _, _] <- crossingP
    connectionP [(a0, b0)]
    guard $ alternatingDefect a0 > 0

    ([c0, c1, c2, c3], _) <- subTangleP 4
    connectionP [(c0, b1), (c1, a3)]

    tangle <- getTangleP

    let pass incoming outcoming = move tangle $ do
            substituteC $ map (\ d -> (d, threadContinuation $ opposite d)) incoming ++ zip (map opposite incoming) outcoming
            connectC $ zip outcoming $ map (threadContinuation . opposite) incoming

    return $ pass [c1, c0] [c2, c3]


patternMatching :: PatternM a (Tangle a) -> Tangle a -> [Tangle a]
patternMatching pat tangle =
    map snd $ runPatternMatching pat $ PatternS tangle (allVertices tangle)
