module Math.Topology.KnotTh.Moves.AdHocOfTangle
    ( flype
    , pass
    , reidemeisterIII
    , weak
    , greedyReidemeisterReduction
    , greedyReidemeisterReductionLink
    , reduce1st
    , reduce2nd
    , smoothA
    , smoothB
    ) where

import Data.List (sort, nub)
import Data.Maybe (mapMaybe)
import Data.Array.Unboxed ((!))
import Control.Monad (when, unless, msum, guard, liftM2)
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Moves.Modify
import Math.Topology.KnotTh.Moves.AdHocOfTangle.Resting


flype :: TangleDiagram -> [TangleDiagram]
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

        ((rp, sq), sub) <-
            restingPart tangle [ba, ca] >>= \ (lst, s) ->
                case lst of
                    [x, y] -> return ((x, y), s)
                    _      -> Nothing

        return $! move tangle $ do
            substituteC [(ba, ae), (ca, ad), (ab, rp), (ac, sq)]
            connectC [(rp, ae), (sq, ad)]
            modifyC True id $ filter ((sub !) . vertexIndex) $ allVertices tangle


pass :: TangleDiagram -> [TangleDiagram]
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
                    tryPass (n + 1) pa' tb (opposite pa' : incoming)
                ]

        passM incoming outcoming = move tangle $ do
            let m = length outcoming
                toRemove = drop m incoming

            substituteC $ map (\ d -> (d, threadContinuation $ opposite d)) incoming ++ zip (map opposite incoming) outcoming
            connectC $ zip outcoming $ map (threadContinuation . opposite) incoming
            unless (null toRemove) $ do
                let p = nextCCW $ opposite $ last toRemove
                let q = opposite $ nextCW $ opposite $ head toRemove
                substituteC [(q, p)]
                maskC $ map endVertex toRemove


reidemeisterIII :: TangleDiagram -> [TangleDiagram]
reidemeisterIII tangle =
    flip mapMaybe (allOutcomingDarts tangle) $ \ ab -> do
        -- \sc           /rb             \sc   /rb
        --  \           /                 \   /
        -- cs\ cb   bc /br               ac\ /ab
        -- ---------------                  /
        --   ca\c   b/ba                 ap/a\aq
        --      \   /         -->         /   \
        --     ac\ /ab                 cs/c   b\br
        --        /                  ---------------
        --     ap/a\aq               ca/ cb   bc \ba
        --      /   \                 /           \
        --   pa/     \qa             /pa           \qa
        guard $ isDart ab

        let ac = nextCCW ab
            ba = opposite ab
            ca = opposite ac

        guard $ isDart ba && isDart ca

        let bc = nextCW ba
            cb = nextCCW ca

        guard $ bc == opposite cb

        let a = beginVertex ab
            b = beginVertex ba
            c = beginVertex ca

        guard $ (a /= b) && (a /= c) && (b /= c)
        guard $ passOver bc == passOver cb

        guard $ let altRoot | passOver ab == passOver ba  = ca
                            | otherwise                   = bc
                in ab < altRoot

        let ap = threadContinuation ab
            aq = nextCW ab
            br = nextCW bc
            cs = nextCCW cb

        return $! move tangle $ do
            substituteC [(ca, ap), (ba, aq), (ab, br), (ac, cs)]
            connectC [(br, aq), (cs, ap)]


weak :: TangleDiagram -> [TangleDiagram]
weak tangle = concat [neighboursBorderCrossing, neighboursBorderLoop]
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

                return $! move tangle $ do
                    maskC [a]
                    if qa == ap
                        then connectC [(xa, ya)] >> emitCircle 1
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

                return $! move tangle $ do
                    substituteC [(abl, ap), (bal, bq)]
                    connectC [(ax, by), (ap, xa), (bq, yb)]


greedyReidemeisterReduction :: TangleDiagram -> TangleDiagram
greedyReidemeisterReduction tangleC = move tangleC $ greedy [reduce1st, reduce2nd]


greedyReidemeisterReductionLink :: LinkDiagram -> LinkDiagram
greedyReidemeisterReductionLink =
    tangleToLink . greedyReidemeisterReduction . linkToTangle


reduce1st :: TangleDiagramDart -> MoveM s DiagramCrossing Bool
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
reduce2nd :: TangleDiagramDart -> MoveM s DiagramCrossing Bool
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


smoothA :: TangleDiagramVertex -> MoveM s DiagramCrossing ()
smoothA cs = do
    let dn@[_, d1, d2, d3] = outcomingDarts cs
    [od0, od1, od2, od3] <- mapM oppositeC dn
    case () of
        _ | od0 == d1 && od3 == d2 -> emitCircle 2
          | od0 == d3 && od1 == d2 -> emitCircle 1
          | od0 == d3              -> connectC [(od1, od2)]
          | od1 == d2              -> connectC [(od0, od3)]
          | otherwise              -> substituteC [(od0, d1), (od3, d2)]
    maskC [cs]


smoothB :: TangleDiagramVertex -> MoveM s DiagramCrossing ()
smoothB cs = do
    let dn@[_, d1, d2, d3] = outcomingDarts cs
    [od0, od1, od2, od3] <- mapM oppositeC dn
    case () of
        _ | od0 == d3 && od1 == d2 -> emitCircle 2
          | od0 == d1 && od3 == d2 -> emitCircle 1
          | od0 == d1              -> connectC [(od2, od3)]
          | od3 == d2              -> connectC [(od0, od1)]
          | otherwise              -> substituteC [(od0, d3), (od1, d2)]
    maskC [cs]
