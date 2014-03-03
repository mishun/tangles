module Math.Topology.KnotTh.EmbeddedLink.Construction
    ( fromLink
    , toLink
    , fromTangleAndStar
    ) where

import qualified Data.Vector.Unboxed as UV
import Math.Combinatorics.ChordDiagram
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link


fromLink :: Link a -> EmbeddedLink a
fromLink = implode . explode


toLink :: EmbeddedLink a -> Link a
toLink sl | eulerChar sl == 2  = implode (explode sl)
          | otherwise          = error "toLink: euler char must be 2"


fromTangleAndStar :: ChordDiagram -> Tangle a -> EmbeddedLink a
fromTangleAndStar cd tangle
    | p /= l     = error "fromTangleAndStar: size conflict"
    | otherwise  = fromTangleAndStar' changeLeg tangle
    where
        p = numberOfPoints cd
        l = numberOfLegs tangle
        a = chordOffsetArray cd

        changeLeg d =
            let i = legPlace d
                j = (i + a UV.! i) `mod` l
            in nthLeg tangle j


{-# INLINE fromTangleAndStar' #-}
fromTangleAndStar' :: (Dart Tangle a -> Dart Tangle a) -> Tangle a -> EmbeddedLink a
fromTangleAndStar' withLeg tangle =
    let watch d | isDart d   = beginPair' d
                | otherwise  = watch $ opposite $ withLeg d
    in implode
        ( numberOfFreeLoops tangle + div (length $ filter (\ l -> opposite l == withLeg l) $ allLegs tangle) 2
        , map (\ v -> (map watch $ incomingDarts v, vertexCrossing v)) $ allVertices tangle
        )
