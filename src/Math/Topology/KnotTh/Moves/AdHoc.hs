module Math.Topology.KnotTh.Moves.AdHoc
    ( reduce1st
    , reduce2nd
    , flype
    , pass
    , smoothA
    , smoothB
    , weak
    ) where

import Data.List (sort, nub)
import Data.Maybe (mapMaybe)
import qualified Data.Vector.Unboxed as UV
import Control.Monad (when, unless, msum, guard, liftM2)
import Math.Topology.KnotTh.Knotted.Threads
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Moves.ModifyDSL
import Math.Topology.KnotTh.Moves.AdHoc.Resting


class (KnottedDiagram k) => AdHocMoves k where
    flype, pass :: k DiagramCrossing -> [k DiagramCrossing] 


reduce1st :: TangleDiagramDart -> ModifyM Tangle DiagramCrossing s Bool
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
reduce2nd :: TangleDiagramDart -> ModifyM Tangle DiagramCrossing s Bool
reduce2nd abl = do
    let a = beginVertex abl
    bal <- oppositeC abl

    if isLeg bal then return False else do
        let abr = nextCCW abl
            bar = nextCW bal
            b = beginVertex bal

        crossingsOk <- liftM2 (==) (passOverC abl) (passOverC bal)
        structureOk <- (== abr) `fmap` oppositeC bar

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
                    then emitLoopsC 1
                    else do
                        when (qa /= ap) $ connectC [(pa, qa)]
                        when (rb /= bs) $ connectC [(rb, sb)]
                else do
                    if qa == br
                        then emitLoopsC 1
                        else connectC [(qa, rb)]

                    if pa == bs
                        then emitLoopsC 1
                        else connectC [(pa, sb)]

            maskC [a, b]
            return True


instance AdHocMoves Tangle where
    flype tangle =
        flip mapMaybe (allOutcomingDarts tangle) $ \ ab -> do
            let ba = opposite ab
                ac = nextCCW ab
                ca = opposite ac

            guard $ isDart ba && isDart ca

            let a = beginVertex ab
                b = beginVertex ba
                c = beginVertex ca

            guard $ b /= c && a /= b && a /= c

            let ae = nextCCW ac
                ad = nextCW ab

            ([rp, sq], sub) <- restingPart tangle [ba, ca]

            return $! modifyKnot tangle $ do
                substituteC [(ba, ae), (ca, ad), (ab, rp), (ac, sq)]
                connectC [(rp, ae), (sq, ad)]
                modifyC True id $ filter ((sub UV.!) . vertexIndex) $ allVertices tangle


    pass tangle = mapMaybe (\ d -> tryPass 1 d d [opposite d]) $ allOutcomingDarts tangle
        where
            tryPass n ha tb incoming = do
                let ah = opposite ha
                guard $ isDart ah

                let hp = nextCW ha
                    ph = opposite hp
                --    tq = nextCCW tb
                --    qt = opposite tq

                msum
                    [ do
                        (outcoming, _) <- restingPart tangle incoming
                        let m = length outcoming
                        guard $ m <= n
                        return $! passM incoming outcoming

                    , do
                        let selfIncoming = map (threadContinuation . opposite) incoming
                        (outcoming, _) <- restingPart tangle selfIncoming
                        let m = length outcoming
                        guard $ m <= n
                        --guard $
                        --    let inside d = isDart d && sub ! crossingIndex (incidentCrossing d)
                        --    in inside qt || inside ph
                        guard $
                            let everything = sort $ map beginVertex $ selfIncoming ++ outcoming
                            in everything == nub everything
                        return $! passM incoming outcoming

                    , do
                        guard $ isDart ph
                        let pa' = nextCW ph
                        guard $ passOver pa' == passOver tb && pa' /= tb
                        guard $ pa' /= head incoming
                        tryPass (n + 1) pa' tb (opposite pa' : incoming)
                    ]

            passM incoming outcoming = modifyKnot tangle $ do
                let m = length outcoming
                    toRemove = drop m incoming

                substituteC $ map (\ d -> (d, threadContinuation $ opposite d)) incoming ++ zip (map opposite incoming) outcoming
                connectC $ zip outcoming $ map (threadContinuation . opposite) incoming
                unless (null toRemove) $ do
                    let p = nextCCW $ opposite $ last toRemove
                    let q = opposite $ nextCW $ opposite $ head toRemove
                    substituteC [(q, p)]
                    maskC $ map endVertex toRemove


instance AdHocMoves Link where
    flype = map tangleToLink . flype . linkToTangle
    pass = map tangleToLink . pass . linkToTangle


smoothA :: TangleDiagramVertex -> ModifyM Tangle DiagramCrossing s ()
smoothA cs = do
    let dn@[_, d1, d2, d3] = outcomingDarts cs
    [od0, od1, od2, od3] <- mapM oppositeC dn
    if | od0 == d1 && od3 == d2 -> emitLoopsC 2
       | od0 == d3 && od1 == d2 -> emitLoopsC 1
       | od0 == d3              -> connectC [(od1, od2)]
       | od1 == d2              -> connectC [(od0, od3)]
       | otherwise              -> substituteC [(od0, d1), (od3, d2)]
    maskC [cs]


smoothB :: TangleDiagramVertex -> ModifyM Tangle DiagramCrossing s ()
smoothB cs = do
    let dn@[_, d1, d2, d3] = outcomingDarts cs
    [od0, od1, od2, od3] <- mapM oppositeC dn
    if | od0 == d3 && od1 == d2 -> emitLoopsC 2
       | od0 == d1 && od3 == d2 -> emitLoopsC 1
       | od0 == d1              -> connectC [(od2, od3)]
       | od3 == d2              -> connectC [(od0, od1)]
       | otherwise              -> substituteC [(od0, d3), (od1, d2)]
    maskC [cs]


weak :: TangleDiagram -> [TangleDiagram]
weak tangle = neighboursBorderCrossing ++ neighboursBorderLoop
    where
        neighboursBorderCrossing =
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

                return $! modifyKnot tangle $ do
                    maskC [a]
                    if qa == ap
                        then connectC [(xa, ya)] >> emitLoopsC 1
                        else connectC [(pa, ya), (qa, xa)]

        neighboursBorderLoop =
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

                return $! modifyKnot tangle $ do
                    substituteC [(abl, ap), (bal, bq)]
                    connectC [(ax, by), (ap, xa), (bq, yb)]
