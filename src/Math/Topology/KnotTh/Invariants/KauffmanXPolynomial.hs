{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.KauffmanXPolynomial
    ( kauffmanXPolynomial
    , minimalKauffmanXPolynomial
    ) where

import Data.Function (fix)
import Data.List (groupBy)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Control.Monad.ST (runST)
import Control.Monad (foldM)
import Math.Topology.KnotTh.Invariants.Util.Poly
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanXStateSum
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.TangleStarGlue


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

    kauffmanXPolynomial link
        | eulerChar link /= 0  = error "jonesPolynomial: yet implemented for torus only"
        | otherwise            =
            let (tangle, star) = splitIntoTangleAndStar link
                l = numberOfLegs tangle

                border :: V.Vector (Int, [Int])
                border = (V.replicate l undefined V.//) $ do
                    (pair, gr) <-
                        let ds =
                                let isBigon d = nextCCW (opposite d) == opposite (nextCW d)
                                in groupBy (const isBigon) $
                                    let (pre, post) = span isBigon (outcomingDarts star)
                                    in post ++ pre
                        in case length ds of
                            4 -> [[1, 0], [0, 1], [-1, 0], [0, -1]] `zip` ds
                            6 -> [[1, 0], [1, 1], [0, 1], [-1, 0], [-1, -1], [0, -1]] `zip` ds
                            _ -> error "internal error"

                    d <- gr
                    return (beginPlace d, (endPlace d, pair))

                homologyMultiple (PlanarChordDiagram a startFactor) = runST $ do
                    visited <- UMV.replicate l False
                    foldM (\ !f !start -> do
                            vs <- UMV.read visited start
                            if vs
                                then return f
                                else do
                                    homology <- fix (\ loop hom !i -> do
                                            c <- UMV.read visited i
                                            if c
                                                then return hom
                                                else do
                                                    let (i', hom') = border V.! i
                                                    UMV.write visited i True
                                                    UMV.write visited i' True
                                                    loop (zipWith (+) hom hom') (a UV.! i')
                                        ) (replicate (2 * genus link) 0) start
                                    return $ f *
                                        if all (== 0) homology
                                            then circleFactor
                                            else monomial 1 "x" 1
                        ) startFactor [0 .. l - 1]

                result =
                    let KauffmanXStateSum _ list = finalNormalization link $ reduceSkeinStd tangle
                    in sum $ map homologyMultiple list
            in result

    minimalKauffmanXPolynomial link =
        min (kauffmanXPolynomial link) (kauffmanXPolynomial $ invertCrossings link)
