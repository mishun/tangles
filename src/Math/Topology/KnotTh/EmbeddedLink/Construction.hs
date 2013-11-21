module Math.Topology.KnotTh.EmbeddedLink.Construction
    ( fromLink
    , toLink
    , fromTangleAndStar
    ) where

import Data.Array.IArray ((!))
import Math.Combinatorics.ChordDiagram
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link


fromLink :: (CrossingType ct) => Link ct -> EmbeddedLink ct
fromLink = implode . explode


toLink :: (CrossingType ct) => EmbeddedLink ct -> Link ct
toLink sl | eulerChar sl == 2  = implode (explode sl)
          | otherwise          = error "toLink: euler char must be 2"


fromTangleAndStar :: (CrossingType ct) => ChordDiagram -> Tangle ct -> EmbeddedLink ct
fromTangleAndStar cd tangle
    | p /= l     = error "fromTangleAndStar: size conflict"
    | otherwise  = fromTangleAndStar' changeLeg tangle
    where
        p = numberOfPoints cd
        l = numberOfLegs tangle
        a = chordOffsetArray cd

        changeLeg d =
            let i = legPlace d
                j = (i + a ! i) `mod` l
            in nthLeg tangle j


{-# INLINE fromTangleAndStar' #-}
fromTangleAndStar' :: (CrossingType ct) => (Dart Tangle ct -> Dart Tangle ct) -> Tangle ct -> EmbeddedLink ct
fromTangleAndStar' withLeg tangle =
    let watch d | isDart d   = beginPair' d
                | otherwise  = watch $ opposite $ withLeg d
    in implode
        ( numberOfFreeLoops tangle + div (length $ filter (\ l -> opposite l == withLeg l) $ allLegs tangle) 2
        , map (\ c -> (map watch $ incomingDarts c, crossingState c)) $ allVertices tangle
        )
