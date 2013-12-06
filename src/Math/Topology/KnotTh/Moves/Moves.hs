module Math.Topology.KnotTh.Moves.Moves
    ( patternMatching
    , flype
    , pass2
    , perko
    ) where

import Control.Monad (guard)
import Debug.Trace
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Moves.PatternMatching
import Math.Topology.KnotTh.Moves.AdHocOfTangle.Move


flype :: PatternM s DiagramCrossing TangleDiagram
flype = do
    ([ab, ac, ae, ad], _) <- crossingP
    ([ca, ba, rp, sq], sub) <- subTangleP 4
    guard $ length sub > 1
    connectionP [(ac, ca), (ab, ba)]

    reconnectP $ do
        substituteC [(ba, ae), (ca, ad), (ab, rp), (ac, sq)]
        connectC [(rp, ae), (sq, ad)]
        modifyC True id sub


pass2 :: PatternM s DiagramCrossing TangleDiagram
pass2 = do
    ([a0, _, _, a3], _) <- crossingP
    ([b0, b1, _, _], _) <- crossingP
    connectionP [(a0, b0)]
    guard $ alternatingDefect a0 > 0
    ([c0, c1, c2, c3], _) <- subTangleP 4
    connectionP [(c0, b1), (c1, a3)]

    reconnectP $
        let pass incoming outcoming = do
                substituteC $ map (\ d -> (d, threadContinuation $ opposite d)) incoming ++ zip (map opposite incoming) outcoming
                connectC $ zip outcoming $ map (threadContinuation . opposite) incoming
        in pass [c1, c0] [c2, c3]


perko :: PatternM s DiagramCrossing TangleDiagram
perko = do
    ([a0, a1, a2, a3], a) <- crossingP
    ([b0, b1, b2, b3], _) <- crossingP
    ([c0, c1, c2, c3], c) <- crossingP
    connectionP [(a0, b0), (b1, c0)]
    guard $ (passOver a0 == passOver b0) && (passOver b1 == passOver c0)

    ([x0, x1, x2, x3, x4, x5], x) <- subTangleP 6
    connectionP [(a1, x0), (b3, x1), (b2, x2), (c3, x3)]

    ([y0, y1, y2, y3], _) <- subTangleP 4
    ([d0, d1, d2, d3], _) <- crossingP
    connectionP [(d2, x4), (y3, x5), (y0, d1), (y1, d0)]
    guard (passOver d0 == passOver a0)

    dst <- reconnectP $ do
        substituteC [(a2, a2), (b1, a3), (b2, c1), (c2, c2), (d1, y2), (y0, d3)]
        connectC [(a3, b0), (b3, c1), (y2, d3), (d2, x4), (a1, x3), (a0, x2), (c0, x1), (c3, x0)]
        modifyC False invertCrossing [a, c]
        modifyC True id x

    trace ("Perko: " ++ show dst) $ return ()
    return dst
