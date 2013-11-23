module Math.Topology.KnotTh.Moves.AdHocOfTangle.Weak
    ( neighbours
    ) where

import Data.Maybe
import Control.Monad (guard)
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Moves.AdHocOfTangle.Move


neighbours :: NATangle -> [NATangle]
neighbours tangle = concatMap ($ tangle) [neighboursBorderCrossing, neighboursBorderLoop]


neighboursBorderCrossing :: NATangle -> [NATangle]
neighboursBorderCrossing tangle =
    flip mapMaybe (allLegs tangle) $ \ xa -> do
        let ax = opposite xa
        guard $ isDart ax

        let ay = nextCCW ax
            ya = nextCCW xa

        guard $ ya == opposite ay

        let a = beginVertex ax
            ap = nextCCW ay
            aq = nextCCW ap
            pa = opposite ap
            qa = opposite aq

        return $! move tangle $ do
            maskC [a]
            if qa == ap
                then connectC [(xa, ya)] >> emitCircle 1
                else connectC [(pa, ya), (qa, xa)]


neighboursBorderLoop :: NATangle -> [NATangle]
neighboursBorderLoop tangle =
    flip mapMaybe (allLegs tangle) $ \ xa -> do
        let ax = opposite xa
        guard $ isDart ax

        let abr = nextCCW ax
            abl = nextCCW abr
            ap = nextCW ax
            bar = opposite abr

        guard $ isDart bar

        let bal = nextCW bar
            by = nextCCW bar
            bq = nextCCW by
            yb = nextCCW xa

        guard $ yb == opposite by
        guard $ abl == opposite bal
        guard $ passOver ax /= passOver by

        return $! move tangle $ do
            substituteC [(abl, ap), (bal, bq)]
            connectC [(ax, by), (ap, xa), (bq, yb)]
