module Math.Topology.KnotTh.Moves.AdHoc.Resting
    ( restingPart
    ) where

import Data.Maybe
import Data.List (find)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV
import Control.Monad.State.Strict (execState, evalState, gets, modify)
import Control.Monad (unless, forM_)
import Math.Topology.KnotTh.Tangle


zeroFlow :: Tangle ct -> UV.Vector Int
zeroFlow tangle = UV.replicate (numberOfDarts tangle) 0


restingPart :: Tangle ct -> [Dart Tangle ct] -> Maybe ([Dart Tangle ct], UV.Vector Bool)
restingPart tangle incoming
    | null incoming       = error "restingPart: no darts"
    | any isLeg incoming  = error "restingPart: leg passed"
    | not startsDifferent = Nothing
    | otherwise           = maxFlow >>= getSubtangle >>= checkConnectivity >>= outcoming
    where
        m = length incoming

        starts = map beginVertex incoming

        ends = mapMaybe maybeEndVertex incoming

        startsDifferent = all (uncurry (/=)) $ zip starts (tail starts)

        maxFlow = push (0 :: Int) flow0
            where
                flow0 =
                    let blockingFlow = concatMap (\ l -> if isLeg l then [] else [(dartIndex l, 1)]) (incoming ++ map opposite incoming)
                    in zeroFlow tangle UV.// blockingFlow

                push flowValue flow
                    | flowValue > m       = Nothing
                    | isNothing nextFlow  = Just (flow, flowValue)
                    | otherwise           = push (flowValue + 1) (fromJust nextFlow)

                    where
                        nextFlow = pushResidualFlow tangle starts ends flow

        getSubtangle (flow, flowValue) = Just (result, flowValue)
            where
                --result = listArray (vertexIndicesRange tangle) $ map (`S.member` subtangle) $ allVertices tangle
                result = UV.fromListN (numberOfVertices tangle + 1) $ False : map (`S.member` subtangle) (allVertices tangle)

                subtangle = execState (forM_ starts dfs) S.empty

                dfs c = do
                    visited <- gets $ S.member c
                    unless visited $ do
                        modify $ S.insert c
                        mapM_ (dfs . endVertex) $ filter (\ d -> (flow UV.! dartIndex d) < 1) $ outcomingDarts c

        checkConnectivity (sub, flowValue)
            | all (`S.member` mask) $ tail starts  = Just (sub, flowValue)
            | otherwise                            = Nothing
            where
                mask = execState (dfs $ head starts) S.empty

                dfs c = do
                    visited <- gets $ S.member c
                    unless visited $ do
                        modify $ S.insert c
                        mapM_ dfs $ filter ((sub UV.!) . vertexIndex) $ mapMaybe maybeEndVertex $ outcomingDarts c

        outcoming (sub, flowValue)
            | any (not . onBorder) incoming  = Nothing
            | flowValue /= length result     = Nothing
            | otherwise                      = Just (result, sub)
            where
                result = restoreOutcoming (opposite lastIncoming) []

                firstIncoming = head incoming
                lastIncoming = last incoming

                onBorder xy = isDart xy && (sub UV.! vertexIndex x) && (isLeg yx || not (sub UV.! vertexIndex y))
                    where
                        yx = opposite xy

                        x = beginVertex xy
                        y = beginVertex yx

                traverseNext = nextCW . opposite

                restoreOutcoming d out
                    | d == firstIncoming  = out
                    | onBorder d          = restoreOutcoming (opposite d) (d : out)
                    | otherwise           = restoreOutcoming (traverseNext d) out


pushResidualFlow :: Tangle ct -> [Vertex Tangle ct] -> [Vertex Tangle ct] -> UV.Vector Int -> Maybe (UV.Vector Int)
pushResidualFlow tangle starts ends flow = evalState bfs initial >>= push
    where
        initial = (Seq.fromList starts, M.fromList $ zip starts (repeat []))

        endFlag = UV.replicate (numberOfVertices tangle + 1) False UV.// map (\ v -> (vertexIndex v, True)) ends

        bfs = do
            empty <- isEmpty
            if empty
                then return Nothing
                else do
                    u <- dequeue
                    if endFlag UV.! vertexIndex u
                        then do
                            p <- getPath u
                            return $ Just p
                        else do
                            let ud = filter (\ d -> flow UV.! dartIndex d < 1) $ outcomingDarts u
                            let brd = find (isLeg . opposite) ud
                            if isJust brd
                                then do
                                    p <- getPath u
                                    return $! Just $! fromJust brd : p
                                else do
                                    mapM_ relax ud
                                    bfs
            where
                relax d = do
                    let v = endVertex d
                    vis <- isVisited v
                    unless vis $ enqueue v d

                isEmpty = gets (\ (q, _) -> Seq.null q)

                dequeue = do
                    (c Seq.:< rest) <- gets (\ (q, _) -> Seq.viewl q)
                    modify (\ (_, p) -> (rest, p))
                    return c

                getPath v = gets (\ (_, p) -> p M.! v)

                enqueue c d = modify (\ (q, p) -> (q Seq.|> c, M.insert c (d : p M.! beginVertex d) p))

                isVisited c = gets (\ (_, p) -> M.member c p)

        push path = return $! flow UV.// pathFlow
            where
                pathFlow = concatMap dartFlow path

                dartFlow d
                    | isLeg r    = [df]
                    | otherwise  = [df, rf]
                    where
                        df = (dartIndex d, (flow UV.! dartIndex d) + 1)
                        r = opposite d
                        rf = (dartIndex r, (flow UV.! dartIndex r) - 1)
