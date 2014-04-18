{-# LANGUAGE RankNTypes #-}
module Math.Topology.KnotTh.Moves.MovesOfELink
    ( movesOfELink
    ) where

import Data.Maybe (mapMaybe)
import Data.List ((\\), subsequences)
import qualified Data.Set as S
import Data.Array.IArray ((!), (//), listArray)
import Data.Array.Unboxed (UArray)
import Control.Monad.State (execState, gets, modify)
import Control.Monad (MonadPlus(..), unless, guard)
import Math.Topology.KnotTh.EmbeddedLink


data PatternS a =
    PatternS
        ([Dart EmbeddedLink a] -> [Dart EmbeddedLink a])
        (EmbeddedLink a)
        [Vertex EmbeddedLink a]


newtype PatternM s a x =
    PatternM
        { runPatternMatching :: PatternS a -> [(PatternS a, x)]
        }


instance Functor (PatternM s a) where
    fmap f x = PatternM (map (fmap f) . runPatternMatching x)


instance Monad (PatternM s a) where
    return x = PatternM (\ s -> [(s, x)])

    (>>=) val act = PatternM (concatMap (\ (s', x) -> runPatternMatching (act x) s') . runPatternMatching val)


instance MonadPlus (PatternM s a) where
    mzero = PatternM (const [])

    mplus a b = PatternM (\ s -> runPatternMatching a s ++ runPatternMatching b s)


subTangleP :: Int -> PatternM s a ([Dart EmbeddedLink a], [Vertex EmbeddedLink a])
subTangleP legs =
    PatternM $ \ (PatternS reorder tangle cs) -> do
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
                in (sub ! vertexIndex x) && not (sub ! vertexIndex y)

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
        return (PatternS reorder tangle (cs \\ subList), (reorder $ drop i borderCCW ++ take i borderCCW, subList))


crossingP :: PatternM s a ([Dart EmbeddedLink a], Vertex EmbeddedLink a)
crossingP =
    PatternM $ \ (PatternS reorder tangle cs) ->
        let try res _ [] = res
            try res skipped (cur : rest) =
                let next = PatternS reorder tangle (skipped ++ rest)
                    sh i = let od = outcomingDarts cur
                           in reorder $ drop i od ++ take i od
                in try ((next, (sh 0, cur)) : (next, (sh 1, cur)) : (next, (sh 2, cur)) : (next, (sh 3, cur)) : res) (cur : skipped) rest
        in try [] [] cs


connectionP :: [(Dart EmbeddedLink a, Dart EmbeddedLink a)] -> PatternM s a ()
connectionP = mapM_ (\ (a, b) -> guard (opposite a == b))


reconnectP :: (Show a) => (forall s. ModifyELinkM a s ()) -> PatternM s' a (EmbeddedLink a)
reconnectP m =
    PatternM $ \ s@(PatternS _ tangle _) ->
        [(s, modifyELink tangle m)]


movesOfELink :: EmbeddedLinkDiagram -> [EmbeddedLinkDiagram]
movesOfELink link = do
    let initial = PatternS id link (allVertices link)
    pattern <- [flype, pass2, handlePass]
    (_, res) <- runPatternMatching pattern initial
    return res


flype :: PatternM s DiagramCrossing EmbeddedLinkDiagram
flype = do
    ([ab, ac, ae, ad], _) <- crossingP
    ([ca, ba, rp, sq], sub) <- subTangleP 4
    guard $ length sub > 1
    connectionP [(ac, ca), (ab, ba)]

    reconnectP $ do
        substituteC [(ba, ae), (ca, ad), (ab, rp), (ac, sq)]
        connectC [(rp, ae), (sq, ad)]
        modifyC True sub


pass2 :: PatternM s DiagramCrossing EmbeddedLinkDiagram
pass2 = do
    ([a0, a1, a2, a3], _) <- crossingP
    ([b0, b1, b2, b3], _) <- crossingP
    connectionP [(a3, b1)]
    guard $ passOver a3 == passOver b1

    mplus
        (do
            ([x0, x1, x2, x3], _) <- subTangleP 4
            connectionP [(x0, a0), (x1, b0)]
            reconnectP $ do
                substituteC [(x0, a2), (x1, b2), (a0, x3), (b0, x2)]
                connectC [(a2, x3), (b2, x2)]
        )
        (do
            ([x0, x1, x2, x3, x4, x5], _) <- subTangleP 6
            connectionP [(x0, a0), (x1, b0), (x5, a1), (x2, b3)]
            reconnectP $ do
                substituteC [(x0, a2), (x1, b2), (a0, x4), (b0, x3)]
                connectC [(a2, x4), (b2, x3)]
        )


handlePass :: PatternM s DiagramCrossing EmbeddedLinkDiagram
handlePass = do
    ([a0, a1, a2, a3], _) <- crossingP
    ([b0, b1, b2, b3], _) <- crossingP
    guard $ passOver a0 == passOver b0

    ([x0, x1, x2, x3, x4, x5], sub) <- subTangleP 6
    connectionP [(a0, x1), (a3, x2), (a1, x3), (b0, x5), (b1, x4), (b2, x0)]

    reconnectP $ do
        substituteC [(a3, a2), (b2, b3)]
        connectC [(a0, x0), (a1, x1), (a2, x5), (b0, x4), (b3, x3), (b1, x2)]
        modifyC True sub
