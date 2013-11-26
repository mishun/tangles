module Math.Topology.KnotTh.Draw.DrawCrossing
    ( DrawableCrossingType(..)
    ) where

import Data.Either (lefts)
import Data.List (groupBy)
import Data.Array.IArray ((!))
import Data.Array (Array)
import Control.Monad (mfilter)
import Math.Topology.KnotTh.Crossings.Projection
import Math.Topology.KnotTh.Crossings.Diagram
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Draw.Settings


cutThread :: (Knotted k) => [(Dart k ct, Dart k ct)]
    -> Array (Dart k ct) (Either [(Double, Double)] ([(Double, Double)], [(Double, Double)]))
        -> (Dart k ct -> Bool) -> ((Maybe (Dart k ct), Maybe (Dart k ct)) -> [(Double, Double)] -> a)
            -> [Either a [(Double, Double)]]

cutThread thread embedding isCut process
    | null thread                    = []
    | hasBreaks && not circleThread  = error "breaks on non-circle thread"
    | hasBreaks                      =
        concatMap processCuts $
            let (pre, post) = break (isBreak . fst) thread
                (h@(a, b)) : t = post
                Right (_, chainB) = embedding ! a
            in matchBreaks [] [((Nothing, Just b), chainB)] (t ++ pre ++ [h])

    | hasCuts                        =
        processCuts $ noBreaks $
            if circleThread
                then let (pre, post) = break (isCut . fst) thread
                     in post ++ pre
                else thread

    | otherwise                      =
        [ Right $ concat $ zipWith ($) (id : repeat tail) $ lefts $ map ((embedding !) . fst) thread ]

    where
        isBreak = either (const False) (const True) . (embedding !)

        circleThread = isDart $ fst $ head thread
        hasBreaks = any (\ (a, b) -> isBreak a || isBreak b) thread
        hasCuts = any (\ (a, b) -> isCut a || isCut b) thread

        matchBreaks chunks chunk ((a, b) : rest) =
            case embedding ! a of
                Left chain             -> matchBreaks chunks (((Just a, Just b), chain) : chunk) rest
                Right (chainA, chainB) ->
                    let nextChunk = ((Just a, Nothing), chainA) : chunk
                    in matchBreaks (reverse nextChunk : chunks) [((Nothing, Just b), chainB)] rest
        matchBreaks chunks _ [] = chunks

        noBreaks = map (\ (a, b) ->
                let Left chain = embedding ! a
                in ((Just a, Just b), chain)
            )

        processCuts chunk =
            let groups = groupBy (\ ((_, b), _) ((a, _), _) ->
                        let isCutM = maybe False isCut
                        in not (isCutM a || isCutM b)
                    ) chunk
            in map (\ gr ->
                    let ((a, _), start) = head gr
                        ((_, b), _) = last gr
                    in Left $ process (mfilter isCut a, mfilter isCut b) $
                        concat $ start : map (tail . snd) (tail gr)
                ) groups


class (ThreadedCrossing ct) => DrawableCrossingType ct where
    crossingDependentSegmentation
        :: (Knotted k) => DrawKnotSettings -> k ct
            -> Array (Dart k ct) (Either [(Double, Double)] ([(Double, Double)], [(Double, Double)]))
                -> [Either [(Double, Double)] [(Double, Double)]]


instance DrawableCrossingType ProjectionCrossingType where
    crossingDependentSegmentation _ knot embedding = do
        thread <- filter (not . null) $ allThreads knot
        cutThread thread embedding (const False) (const id)


instance DrawableCrossingType DiagramCrossingType where
    crossingDependentSegmentation s knot embedding = do
        thread <- filter (not . null) $ allThreads knot
        cutThread thread embedding
            (\ d -> isDart d && passUnder d)
            (\ (a, b) chain ->
                let n = length chain

                    update Nothing p _ = p
                    update (Just _) (x0, y0) (x1, y1) =
                        let dx = x1 - x0
                            dy = y1 - y0
                            m = min 1.0 $ 3.0 * threadWidth s / sqrt (dx * dx + dy * dy)
                        in (x0 + m * dx, y0 + m * dy)

                in [ update a (head chain) (chain !! 1) ]
                    ++ take (n - 2) (tail chain)
                        ++ [ update b (chain !! (n - 1)) (chain !! (n - 2)) ]
            )
