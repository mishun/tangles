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


connectionNonAltP :: [(EmbeddedLinkDiagramDart, EmbeddedLinkDiagramDart)] -> PatternM s DiagramCrossing ()
connectionNonAltP = mapM_ (\ (a, b) -> guard (opposite a == b && passOver a == passOver b))


reconnectP :: (Show a) => (forall s. ModifyELinkM a s ()) -> PatternM s' a (EmbeddedLink a)
reconnectP m =
    PatternM $ \ s@(PatternS _ tangle _) ->
        [(s, modifyELink tangle m)]


movesOfELink :: EmbeddedLinkDiagram -> [EmbeddedLinkDiagram]
movesOfELink link = do
    (pattern, reorders) <-
        [ (flype, [id])
        , (pass2, [id])
        , (pass1, [id])
        , (pass3, [id])
        , (handleFlype, [id, reverse])
        , (handleTwist, [id, reverse])
        , (handlePass2, [id, reverse])
        ]
    reorder <- reorders
    let initial = PatternS reorder link (allVertices link)
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


pass1 :: PatternM s DiagramCrossing EmbeddedLinkDiagram
pass1 = do
    ([a0, a1, a2, a3], _) <- crossingP
    mplus
        (do
            ([x0, x1], _) <- subTangleP 2
            connectionP [(a0, x0)]
            reconnectP $ do
                substituteC [(x0, a2), (a0, x1)]
                connectC [(x1, a2)]
        )
        (do
            ([x0, x1, x2, x3], _) <- subTangleP 4
            connectionP [(a0, x0), (a1, x3), (a3, x1)]
            reconnectP $ do
                substituteC [(x0, a2), (a0, x2)]
                connectC [(x2, a2)]
        )


pass2 :: PatternM s DiagramCrossing EmbeddedLinkDiagram
pass2 = do
    ([a0, a1, a2, a3], _) <- crossingP
    ([b0, b1, b2, b3], _) <- crossingP
    connectionNonAltP [(a3, b1)]

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


pass3 :: PatternM s DiagramCrossing EmbeddedLinkDiagram
pass3 = do
    ([a0, a1, a2, a3], _) <- crossingP
    ([b0, b1, b2, b3], _) <- crossingP
    connectionNonAltP [(a3, b1)]
    ([c0, c1, c2, c3], _) <- crossingP
    connectionNonAltP [(b3, c1)]

    mplus
        (do
            ([x0, x1, x2, x3, x4, x5], _) <- subTangleP 6
            connectionP [(x0, a0), (x1, b0), (x2, c0)]
            reconnectP $ do
                substituteC [(x0, a2), (x1, b2), (x2, c2), (a0, x5), (b0, x4), (c0, x3)]
                connectC [(a2, x5), (b2, x4), (c2, x3)]
        )
        (do
            ([x0, x1, x2, x3, x4, x5, x6, x7], _) <- subTangleP 8
            connectionP [(x0, a0), (x1, b0), (x2, c0), (x7, a1), (x3, c3)]
            reconnectP $ do
                substituteC [(x0, a2), (x1, b2), (x2, c2), (a0, x6), (b0, x5), (c0, x4)]
                connectC [(a2, x6), (b2, x5), (c2, x4)]
        )


handleFlype :: PatternM s DiagramCrossing EmbeddedLinkDiagram
handleFlype = do
    ([a0, a1, a2, a3], _) <- crossingP
    ([b0, b1, b2, b3], _) <- crossingP
    guard $ passOver a0 == passOver b0

    ([x0, x1, x2, x3, x4, x5], sub) <- subTangleP 6
    connectionP [(a0, x1), (a3, x2), (a1, x3), (b0, x5), (b1, x4), (b2, x0)]

    reconnectP $ do
        substituteC [(a3, a2), (b2, b3)]
        connectC [(a0, x0), (a1, x1), (a2, x5), (b0, x4), (b3, x3), (b1, x2)]
        modifyC True sub


handleTwist :: PatternM s DiagramCrossing EmbeddedLinkDiagram
handleTwist = do
    ([a0, a1, a2, a3], a) <- crossingP
    ([b0, b1, b2, b3], _) <- crossingP
    connectionNonAltP [(a2, b0)]
    ([c0, c1, c2, c3], _) <- crossingP
    connectionNonAltP [(a3, c0)]

    ([x0, x1, x2, x3, x4, x5], sub) <- subTangleP 6
    connectionP [(x0, b3), (x1, b1), (x2, a1), (x3, a0), (x4, c3), (x5, c1)]

    reconnectP $ do
        substituteC [(b0, c2), (c0, b2)]
        connectC [(x2, b2), (x3, c2)]
        maskC [a]
        modifyC True sub


handlePass2 :: PatternM s DiagramCrossing EmbeddedLinkDiagram
handlePass2 = do
    ([a0, a1, a2, a3], _) <- crossingP
    ([b0, b1, b2, b3], _) <- crossingP
    connectionNonAltP [(a3, b1), (a1, b3)]

    ([x0, x1, x2, x3, x4, x5], _) <- subTangleP 6
    connectionP [(x0, a0), (x1, b0), (x2, x5)]

    reconnectP $ do
        substituteC [(x0, a2), (x1, b2), (a0, x4), (b0, x3)]
        connectC [(a2, x4), (b2, x3)]

