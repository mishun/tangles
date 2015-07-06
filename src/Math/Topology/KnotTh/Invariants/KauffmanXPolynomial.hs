{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.KauffmanXPolynomial
    ( kauffmanXPolynomial
    , minimalKauffmanXPolynomial
    ) where

import Data.List (partition)
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV
import Math.Topology.KnotTh.Invariants.Util.Poly
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanXStateSum
import Math.Topology.KnotTh.Invariants.KnotPolynomials.Surface
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.Tangle


class (Knotted k) => KnottedWithKauffmanXPolynomial k where
    type KauffmanXPolynomial k :: *
    kauffmanXPolynomial        :: k DiagramCrossing -> KauffmanXPolynomial k
    minimalKauffmanXPolynomial :: k DiagramCrossing -> KauffmanXPolynomial k


instance KnottedWithKauffmanXPolynomial Tangle where
    type KauffmanXPolynomial Tangle = KauffmanXStateSum Poly
    kauffmanXPolynomial tangle = finalNormalization tangle (reduceSkeinStd tangle)
    minimalKauffmanXPolynomial = skeinRelationPostMinimization kauffmanXPolynomial


instance KnottedWithKauffmanXPolynomial Link where
    type KauffmanXPolynomial Link = Poly
    kauffmanXPolynomial = takeAsScalar . kauffmanXPolynomial . linkToTangle
    minimalKauffmanXPolynomial = takeAsScalar . minimalKauffmanXPolynomial . linkToTangle


instance KnottedWithKauffmanXPolynomial EmbeddedLink where
    type KauffmanXPolynomial EmbeddedLink = Poly2

    kauffmanXPolynomial link | numberOfVertices link == 0  = 1
                             | otherwise                   =
        case eulerCharOf link of
            2 -> kauffmanXPolynomial (toLink link)

            0 -> sum $ do
                let (dim, weightedLoopSystems) = homologyDecomposition link

                    tab = filter ((/= 0) . snd) $ M.assocs $
                        foldl (\ m (k, v) -> M.insertWith' (+) k v m) M.empty $ do
                            (loopSystem, weight) <- weightedLoopSystems
                            let (trivial, nonTrivial) = partition (UV.all (== 0)) loopSystem
                                [x, y] = UV.toList $ foldl (UV.zipWith (+)) (UV.replicate dim 0) nonTrivial
                            return ((x, y), weight * (loopFactor ^ length trivial))

                ((x, y), weight) <- min (torusMinimization tab)
                                        (torusMinimization $ map (\ ((x, y), a) -> ((x, -y), a)) tab)

                return $ weight * monomial 1 "g" (fromIntegral x) * monomial 1 "h" (fromIntegral y)

            _ -> sum $ do
                let (_, weightedLoopSystems) = homologyDecomposition link
                (loopSystem, weight) <- weightedLoopSystems
                let (trivial, nonTrivial) = partition (UV.all (== 0)) loopSystem
                return $ weight * (loopFactor ^ length trivial) * monomial 1 "x" (fromIntegral $ length nonTrivial)

    minimalKauffmanXPolynomial link =
        min (kauffmanXPolynomial link) (kauffmanXPolynomial $ invertCrossings link)
