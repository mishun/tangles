module Math.Topology.KnotTh.Moves.AdHocOfTangle.ReidemeisterReduction
    ( greedy1st2ndReduction
    , reduce1st
    , reduce2nd
    ) where

import Control.Monad (when, liftM2)
import Control.Applicative ((<$>))
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Moves.AdHocOfTangle.Move


greedy1st2ndReduction :: NATangle -> NATangle
greedy1st2ndReduction tangleC = move tangleC $ greedy [reduce1st, reduce2nd]


reduce1st :: NATangleDart -> MoveM s ArbitraryCrossing Bool
reduce1st aad = do
    aar <- oppositeC aad
    if aar /= nextCCW aad
        then return False
        else do
            let ab = threadContinuation aad
            let ac = nextCW aad
            ba <- oppositeC ab
            substituteC [(ba, ac)]
            maskC [beginVertex aad]
            return True


--     \ /
--      /
--     /b\
-- bar/   \bal
--   (     )
-- abr\   /abl
--     \a/
--      \
--     / \
reduce2nd :: NATangleDart -> MoveM s ArbitraryCrossing Bool
reduce2nd abl = do
    let a = beginVertex abl
    bal <- oppositeC abl

    if isLeg bal then return False else do
        let abr = nextCCW abl
            bar = nextCW bal
            b = beginVertex bal

        crossingsOk <- liftM2 (==) (passOverC abl) (passOverC bal)
        structureOk <- (== abr) <$> oppositeC bar

        if not (structureOk && (a /= b) && crossingsOk) then return False else do
            let ap = threadContinuation abl
                aq = nextCW abl
                br = nextCCW bal
                bs = threadContinuation bal

            pa <- oppositeC ap
            qa <- oppositeC aq
            rb <- oppositeC br
            sb <- oppositeC bs

            if qa == ap || rb == bs
                then if qa == ap && rb == bs
                    then emitCircle 1
                    else do
                        when (qa /= ap) $ connectC [(pa, qa)]
                        when (rb /= bs) $ connectC [(rb, sb)]
                else do
                    if qa == br
                        then emitCircle 1
                        else connectC [(qa, rb)]

                    if pa == bs
                        then emitCircle 1
                        else connectC [(pa, sb)]

            maskC [a, b]
            return True
