{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.KauffmanXPolynomial
    ( kauffmanXPolynomial
    , minimalKauffmanXPolynomial
    ) where

import Data.Function (fix)
import Data.List (groupBy)
import Data.Array.IArray (array, (!))
import Data.Array.MArray (newArray, readArray, writeArray)
import Data.Array (Array)
import Data.Array.ST (STUArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (foldM)
import Math.Topology.KnotTh.Invariants.Util.Poly
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanXStateSum
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.TangleStarGlue


class (Knotted k) => KnottedWithKauffmanXPolynomial k where
    type KauffmanXPolynomial k :: *
    kauffmanXPolynomial        :: k ArbitraryCrossing -> KauffmanXPolynomial k
    minimalKauffmanXPolynomial :: k ArbitraryCrossing -> KauffmanXPolynomial k


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

                border :: Array Int (Int, (Int, Int))
                border = array (0, l - 1) $ do
                    (pair, gr) <-
                        let ds =
                                let isBigon d = nextCCW (opposite d) == opposite (nextCW d)
                                in groupBy (const isBigon) $
                                    let (pre, post) = span isBigon (outcomingDarts star)
                                    in post ++ pre
                        in case length ds of
                            4 -> [(1, 0), (0, 1), (-1, 0), (0, -1)] `zip` ds
                            6 -> [(1, 0), (1, 1), (0, 1), (-1, 0), (-1, -1), (0, -1)] `zip` ds
                            _ -> error "internal error"

                    d <- gr
                    return (beginPlace d, (endPlace d, pair))

                homotopyMultiple (PlanarChordDiagram a startFactor) = runST $ do
                    visited <- newArray (0, l - 1) False :: ST s (STUArray s Int Bool)
                    foldM (\ !f !start -> do
                            vs <- readArray visited start
                            if vs
                                then return f
                                else do
                                    pair <- fix (\ loop (!p, !q) !i -> do
                                            c <- readArray visited i
                                            if c
                                                then return (p, q)
                                                else do
                                                    let (i', (dp, dq)) = border ! i
                                                    writeArray visited i True
                                                    writeArray visited i' True
                                                    loop (p + dp, q + dq) (a ! i')
                                        ) (0, 0) start
                                    return $ f *
                                        (case pair of
                                            (0, 0) -> circleFactor
                                            _      -> monomial 1 "x" 1
                                        )
                        ) startFactor [0 .. l - 1]

                result =
                    let KauffmanXStateSum _ list = finalNormalization link $ reduceSkeinStd tangle
                    in sum $ map homotopyMultiple list
            in result

    minimalKauffmanXPolynomial link =
        min (kauffmanXPolynomial link) (kauffmanXPolynomial $ invertCrossings link)
