{-# LANGUAGE FlexibleContexts #-}
module Math.Topology.KnotTh.Moves.AdHoc.Resting
    ( restingPart
    ) where

import Control.Monad (unless, forM_)
import qualified Control.Monad.State.Strict as State
import Data.List (find)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import qualified Data.Vector.Unboxed as UV
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
                    | otherwise           =
                        case pushResidualFlow tangle starts ends flow of
                            Nothing       -> Just (flow, flowValue)
                            Just nextFlow -> push (flowValue + 1) nextFlow

        getSubtangle (flow, flowValue) = Just (result, flowValue)
            where
                --result = listArray (vertexIndicesRange tangle) $ map (`S.member` subtangle) $ allVertices tangle
                result = UV.fromListN (numberOfVertices tangle + 1) $ False : map (`Set.member` subtangle) (allVertices tangle)

                subtangle = State.execState (forM_ starts dfs) Set.empty

                dfs c = do
                    visited <- State.gets $ Set.member c
                    unless visited $ do
                        State.modify' $ Set.insert c
                        mapM_ (dfs . endVertex) $ filter (\ d -> (flow UV.! dartIndex d) < 1) $ outcomingDarts c

        checkConnectivity (sub, flowValue)
            | all (`Set.member` mask) $ tail starts  = Just (sub, flowValue)
            | otherwise                              = Nothing
            where
                mask = State.execState (dfs $ head starts) Set.empty

                dfs c = do
                    visited <- State.gets $ Set.member c
                    unless visited $ do
                        State.modify' $ Set.insert c
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
pushResidualFlow tangle starts ends flow = State.evalState bfs initial >>= push
    where
        initial = (Seq.fromList starts, Map.fromList $ zip starts (repeat []))

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
                            case find (isLeg . opposite) ud of
                                Just brd -> do
                                    p <- getPath u
                                    return $! Just $! brd : p
                                Nothing  -> do
                                    mapM_ relax ud
                                    bfs
            where
                relax d = do
                    let v = endVertex d
                    vis <- isVisited v
                    unless vis $ enqueue v d

                isEmpty = State.gets (Seq.null . fst)

                dequeue = do
                    (c Seq.:< rest) <- State.gets (\ (q, _) -> Seq.viewl q)
                    State.modify' (\ (_, p) -> (rest, p))
                    return c

                getPath v = State.gets (\ (_, p) -> p Map.! v)

                enqueue c d = State.modify' (\ (q, p) -> (q Seq.|> c, Map.insert c (d : p Map.! beginVertex d) p))

                isVisited c = State.gets (Map.member c . snd)

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
