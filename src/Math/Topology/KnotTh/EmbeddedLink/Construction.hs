module Math.Topology.KnotTh.EmbeddedLink.Construction
    ( fromLink
    , toLink
    , fromTangleAndStar
    , twistedDoubleSatelliteELink
    ) where

import Data.Array.IArray ((!))
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Control.Monad (when)
import Math.Combinatorics.ChordDiagram
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link


fromLink :: Link a -> EmbeddedLink a
fromLink = implode . explode


toLink :: EmbeddedLink a -> Link a
toLink sl | eulerChar sl == 2         = l
          | numberOfVertices sl == 0  = l
          | otherwise                 = error "toLink: euler char must be 2"
    where
        l = implode $ explode sl


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


twistedDoubleSatelliteELink :: EmbeddedLinkDiagram -> EmbeddedLinkDiagram
twistedDoubleSatelliteELink = twistedNSatellite 2


twistedNSatellite :: Int -> EmbeddedLinkDiagram -> EmbeddedLinkDiagram
twistedNSatellite n link
    | n < 0      = error "twistedNSattelite: negative order"
    | n == 0     = emptyKnotted
    | n == 1     = link
    | otherwise  = tensorSubstELink n wrap link
    where
        w = selfWritheArray link

        wrap v | wc == 0    = cross
               | otherwise  =
                   let r | wc > 0     = underCrossing
                         | otherwise  = overCrossing

                       braid =
                           let half = reversingBraidTangle n r
                           in half |=| half
                   in glueTangles n (nthLeg braid n) (nthLeg cross $ n - 1)
            where
                wc = w ! v
                s = vertexCrossing v
                cross = gridTangle (n, n) (const s)


tensorSubstELink :: Int -> (Vertex EmbeddedLink a -> Tangle b) -> EmbeddedLink a -> EmbeddedLink b
tensorSubstELink k crossF link = implode (k * numberOfFreeLoops link, body)
    where
        n = numberOfVertices link

        crossSubst =
            let substList = do
                    c <- allVertices link
                    let t = crossF c
                    when (numberOfLegs t /= 4 * k) $
                        fail "bad number of legs"
                    return $! t
            in V.fromListN (n + 1) $ undefined : substList

        crossOffset = UV.fromListN (n + 1) $
            0 : scanl (\ !p !i -> p + numberOfVertices (crossSubst V.! i)) 0 [1 .. n]

        resolveInCrossing !v !d
            | isLeg d    =
                let p = legPlace d
                in resolveOutside (opposite $ nthOutcomingDart v $ p `div` k) (p `mod` k)
            | otherwise  =
                let (c, p) = beginPair' d
                in ((crossOffset UV.! vertexIndex v) + c, p)

        resolveOutside !d !i =
            let (c, p) = beginPair d
            in resolveInCrossing c $ opposite $
                    nthLeg (crossSubst V.! vertexIndex c) (k * p + k - 1 - i)

        body = do
            c <- allVertices link
            let t = crossSubst V.! vertexIndex c
            c' <- allVertices t
            return (map (resolveInCrossing c) $ incomingDarts c', vertexCrossing c')
