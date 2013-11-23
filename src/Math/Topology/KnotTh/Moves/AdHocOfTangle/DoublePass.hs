module Math.Topology.KnotTh.Moves.AdHocOfTangle.DoublePass
    ( neighbours
    ) where

import Data.Maybe
import Debug.Trace
import Control.Monad (guard)
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Moves.AdHocOfTangle.Resting


neighbours :: NATangle -> [NATangle]
neighbours tangle =
    flip mapMaybe (allOutcomingDarts tangle) $ \ ab -> do
        let ba = opposite ab
            bc = nextCCW ba
            cb = opposite bc
            cd = nextCW cb
            dc = opposite cd

        guard $ isDart ba && isDart cb && isDart dc

        let incomingA = map opposite [threadContinuation ab, nextCW ab, nextCCW cb, threadContinuation cb]
            incomingB = map opposite [threadContinuation dc, nextCW dc, nextCCW bc, threadContinuation bc]

        guard $ all isDart incomingA
        guard $ all isDart incomingB

        (outcomingA, _) <- restingPart tangle incomingA
        (outcomingB, _) <- restingPart tangle incomingB

        guard $ length outcomingA == 2
        guard $ length outcomingB == 2

        trace "Double Pass found" Nothing
