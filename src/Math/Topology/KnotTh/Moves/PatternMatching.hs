{-# LANGUAGE RankNTypes #-}
module Math.Topology.KnotTh.Moves.PatternMatching
    ( PatternM
    , subTangleP
    , crossingP
    , connectionP
    , reconnectP
    , patternMatching
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

newtype PatternM s a x = PatternM { runPatternMatching :: PatternS a -> [(PatternS a, x)] }


instance Functor (PatternM s a) where
    fmap f x = PatternM (map (fmap f) . runPatternMatching x)


instance Monad (PatternM s a) where
    return x = PatternM (\ s -> [(s, x)])

    (>>=) val act = PatternM (concatMap (\ (s', x) -> runPatternMatching (act x) s') . runPatternMatching val)


instance MonadPlus (PatternM s a) where
    mzero = PatternM (const [])

    mplus a b = PatternM (\ s -> runPatternMatching a s ++ runPatternMatching b s)


subTangleP :: Int -> PatternM s a ([Dart Tangle a], [Vertex Tangle a])
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
                        mapM_ dfs $
                            filter ((sub !) . vertexIndex) $
                                mapMaybe endVertexM $ outcomingDarts c
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


crossingP :: PatternM s a ([Dart Tangle a], Vertex Tangle a)
crossingP =
    PatternM $ \ (PatternS tangle cs) ->
        let try res _ [] = res
            try res skipped (cur : rest) =
                let next = PatternS tangle (skipped ++ rest)
                    sh i = let od = outcomingDarts cur
                           in drop i od ++ take i od
                in try ((next, (sh 0, cur)) : (next, (sh 1, cur)) : (next, (sh 2, cur)) : (next, (sh 3, cur)) : res) (cur : skipped) rest
        in try [] [] cs


connectionP :: [(Dart Tangle a, Dart Tangle a)] -> PatternM s a ()
connectionP = mapM_ (\ (a, b) -> guard (opposite a == b))


reconnectP :: (Show a) => (forall s. MoveM s a ()) -> PatternM s' a (Tangle a)
reconnectP m =
    PatternM $ \ s@(PatternS tangle _) ->
        [(s, move tangle m)]


patternMatching :: (forall s. [PatternM s a (Tangle a)]) -> Tangle a -> [Tangle a]
patternMatching patterns tangle = do
    let initial = PatternS tangle (allVertices tangle)
    pattern <- patterns
    (_, res) <- runPatternMatching pattern initial
    return res
