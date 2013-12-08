module Math.Topology.KnotTh.Moves.Moves
    ( searchMoves
    , flype
    , pass1
    , pass2
    , pass3
    , perko
    , doublePass
    ) where

import Control.Monad (mplus, guard, when)
import Text.Printf
import Debug.Trace
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Moves.PatternMatching
import Math.Topology.KnotTh.Moves.AdHocOfTangle.Move
import Math.Topology.KnotTh.Invariants


flype :: Pattern DiagramCrossing TangleDiagram
flype = makePattern $ do
    ([ab, ac, ae, ad], _) <- crossingP
    ([ca, ba, rp, sq], sub) <- subTangleP 4
    guard $ length sub > 1
    connectionP [(ac, ca), (ab, ba)]

    reconnectP $ do
        substituteC [(ba, ae), (ca, ad), (ab, rp), (ac, sq)]
        connectC [(rp, ae), (sq, ad)]
        modifyC True id sub


pass1 :: Pattern DiagramCrossing TangleDiagram
pass1 = makePattern $ do
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


pass2 :: Pattern DiagramCrossing TangleDiagram
pass2 = makePattern $ do
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


pass3 :: Pattern DiagramCrossing TangleDiagram
pass3 = makePattern $ do
    ([a0, a1, a2, a3], _) <- crossingP
    ([b0, b1, b2, b3], _) <- crossingP
    connectionP [(a3, b1)]
    guard $ passOver a3 == passOver b1
    ([c0, c1, c2, c3], _) <- crossingP
    connectionP [(b3, c1)]
    guard $ passOver b3 == passOver c1

    mplus
        (do
            ([x0, x1, x2, x3, x4, x5], _) <- subTangleP 6
            connectionP [(x0, a0), (x1, b0), (x2, c0)]
            trace "Pass3" $ return ()
            reconnectP $ do
                substituteC [(x0, a2), (x1, b2), (x2, c2), (a0, x5), (b0, x4), (c0, x3)]
                connectC [(a2, x5), (b2, x4), (c2, x3)]
        )
        (do
            ([x0, x1, x2, x3, x4, x5, x6, x7], _) <- subTangleP 8
            connectionP [(x0, a0), (x1, b0), (x2, c0), (x7, a1), (x3, c3)]
            trace "SelfPass3" $ return ()
            reconnectP $ do
                substituteC [(x0, a2), (x1, b2), (x2, c2), (a0, x6), (b0, x5), (c0, x4)]
                connectC [(a2, x6), (b2, x5), (c2, x4)]
        )


perko :: Pattern DiagramCrossing TangleDiagram
perko = makePattern $ do
    ([a0, a1, a2, a3], a) <- crossingP
    ([b0, b1, b2, b3], b) <- crossingP
    ([c0, c1, c2, c3], c) <- crossingP
    connectionP [(a0, b0), (b1, c0)]
    guard $ (passOver a0 == passOver b0) && (passOver b1 == passOver c0)

    ([x0, x1, x2, x3, x4, x5], x) <- subTangleP 6
    connectionP [(a1, x0), (b3, x1), (b2, x2), (c3, x3)]

    ([y0, y1, y2, y3], y) <- subTangleP 4
    ([d0, d1, d2, d3], d) <- crossingP
    connectionP [(d2, x4), (y3, x5), (y0, d1), (y1, d0)]
    guard $ passOver d0 == passOver a0

    reconnectP $ do
        substituteC [(b1, a3), (b2, c1), (d1, y2), (y0, d3)]
        connectC [(a3, b0), (b3, c1), (y2, d3), (d2, x4), (a1, x3), (a0, x2), (c0, x1), (c3, x0)]
        modifyC False invertCrossing [a, c]
        modifyC True id x


doublePass :: Pattern DiagramCrossing TangleDiagram
doublePass = makePattern $ do
    ([a0, a1, a2, a3], a) <- crossingP
    ([b0, b1, b2, b3], b) <- crossingP
    connectionP [(a0, b0)]
    guard $ passOver a0 == passOver b0
    ([c0, c1, c2, c3], c) <- crossingP
    connectionP [(b1, c1)]
    guard $ passOver b1 /= passOver c1
    ([d0, d1, d2, d3], d) <- crossingP
    connectionP [(c0, d0)]
    guard $ passOver c0 == passOver d0

    ([x0, x1, x2, x3, x4, x5], x) <- subTangleP 6
    connectionP [(x0, c3), (x1, c2), (x2, a3), (x3, a2)]

    ([y0, y1, y2, y3, y4, y5], y) <- subTangleP 6
    connectionP [(y0, b3), (y1, b2), (y2, d3), (y3, d2)]

    reconnectP $ do
        substituteC [(x2, a1), (a3, x4), (b1, x5), (y2, d1), (d3, y4), (c1, y5)]
        connectC [(x0, y0), (a1, x4), (b3, x5), (b2, y1), (d1, y4), (c3, y5), (c2, x1)]
