module Math.Topology.KnotTh.Tangle.Moves.Resting
    ( restingPart
    ) where

import Data.Maybe
import Data.List (find)
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Array.Unboxed (UArray, (!), (//), array, listArray)
import Control.Monad.State.Strict (execState, evalState, gets, modify)
import Control.Monad (unless, forM_)
import Control.Arrow (first)
import Math.Topology.KnotTh.Tangle


zeroFlow :: Tangle ct -> UArray Int Int
zeroFlow tangle = listArray (dartIndexRange tangle) $ repeat 0


restingPart :: Tangle ct -> [Dart Tangle ct] -> Maybe ([Dart Tangle ct], UArray Int Bool)
restingPart tangle incoming
    | null incoming       = error "restingPart: no darts"
    | any isLeg incoming  = error "restingPart: leg passed"
    | not startsDifferent = Nothing
    | otherwise           = maxFlow >>= getSubtangle >>= checkConnectivity >>= outcoming
    where
        m = length incoming

        starts = map incidentCrossing incoming

        ends = mapMaybe maybeAdjacentCrossing incoming

        startsDifferent = all (uncurry (/=)) $ zip starts (tail starts)

        maxFlow = push (0 :: Int) flow0
            where
                flow0 =
                    let blockingFlow = concatMap (\ l -> if isLeg l then [] else [(dartIndex l, 1)]) (incoming ++ map opposite incoming)
                    in zeroFlow tangle // blockingFlow

                push flowValue flow
                    | flowValue > m       = Nothing
                    | isNothing nextFlow  = Just (flow, flowValue)
                    | otherwise           = push (flowValue + 1) (fromJust nextFlow)

                    where
                        nextFlow = pushResidualFlow tangle starts ends flow

        getSubtangle (flow, flowValue) = Just (result, flowValue)
            where
                result = listArray (crossingIndexRange tangle) $ map (`S.member` subtangle) $ allCrossings tangle

                subtangle = execState (forM_ starts dfs) S.empty

                dfs c = do
                    visited <- gets $ S.member c
                    unless visited $ do
                        modify $ S.insert c
                        mapM_ (dfs . adjacentCrossing) $ filter (\ d -> (flow ! dartIndex d) < 1) $ incidentDarts c

        checkConnectivity (sub, flowValue)
            | all (`S.member` mask) $ tail starts  = Just (sub, flowValue)
            | otherwise                            = Nothing
            where
                mask = execState (dfs $ head starts) S.empty

                dfs c = do
                    visited <- gets $ S.member c
                    unless visited $ do
                        modify $ S.insert c
                        mapM_ dfs $ filter ((sub !) . crossingIndex) $ mapMaybe maybeAdjacentCrossing $ incidentDarts c

        outcoming (sub, flowValue)
            | any (not . onBorder) incoming  = Nothing
            | flowValue /= length result     = Nothing
            | otherwise                      = Just (result, sub)
            where
                result = restoreOutcoming (opposite lastIncoming) []

                firstIncoming = head incoming
                lastIncoming = last incoming

                onBorder xy = isDart xy && (sub ! crossingIndex x) && (isLeg yx || not (sub ! crossingIndex y))
                    where
                        yx = opposite xy

                        x = incidentCrossing xy
                        y = incidentCrossing yx

                traverseNext = nextCW . opposite

                restoreOutcoming d out
                    | d == firstIncoming  = out
                    | onBorder d          = restoreOutcoming (opposite d) (d : out)
                    | otherwise           = restoreOutcoming (traverseNext d) out


pushResidualFlow :: Tangle ct -> [Crossing Tangle ct] -> [Crossing Tangle ct] -> UArray Int Int -> Maybe (UArray Int Int)
pushResidualFlow tangle starts ends flow = evalState bfs initial >>= push
    where
        initial = (Seq.fromList starts, M.fromList $ zip starts (repeat []))

        endFlag :: UArray Int Bool
        endFlag = array (crossingIndexRange tangle) $
            map (first crossingIndex) $
                zip (allCrossings tangle) (repeat False) ++ zip ends (repeat True)

        bfs = do
            empty <- isEmpty
            if empty
                then return Nothing
                else do
                    u <- dequeue
                    if endFlag ! crossingIndex u
                        then do
                            p <- getPath u
                            return $! Just p
                        else do
                            let ud = filter (\ d -> flow ! dartIndex d < 1) $ incidentDarts u
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
                    let v = adjacentCrossing d
                    vis <- isVisited v
                    unless vis $ enqueue v d

                isEmpty = gets (\ (q, _) -> Seq.null q)

                dequeue = do
                    (c Seq.:< rest) <- gets (\ (q, _) -> Seq.viewl q)
                    modify (\ (_, p) -> (rest, p))
                    return c

                getPath v = gets (\ (_, p) -> p M.! v)

                enqueue c d = modify (\ (q, p) -> (q Seq.|> c, M.insert c (d : p M.! incidentCrossing d) p))

                isVisited c = gets (\ (_, p) -> M.member c p)

        push path = return $! flow // pathFlow
            where
                pathFlow = concatMap dartFlow path

                dartFlow d
                    | isLeg r    = [df]
                    | otherwise  = [df, rf]
                    where
                        df = (dartIndex d, (flow ! dartIndex d) + 1)
                        r = opposite d
                        rf = (dartIndex r, (flow ! dartIndex r) - 1)
