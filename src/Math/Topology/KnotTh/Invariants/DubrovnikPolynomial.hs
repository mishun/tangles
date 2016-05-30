{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, MultiWayIf, TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.DubrovnikPolynomial
    ( BirmanWenzlAlgebra
    , dubrovnikPolynomial
    , minimalDubrovnikPolynomial
    , kauffmanFPolynomial
    , normalizedKauffmanFPolynomialOfLink
    ) where

import Control.DeepSeq (NFData(..))
import qualified Data.Array as A
import Data.Function (on)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import qualified Data.Vector.Unboxed as UV
import Text.Printf
import Math.Topology.KnotTh.Algebra
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.LinkingNumbers
import qualified Math.Topology.KnotTh.Invariants.Util.Poly as P
import Math.Topology.KnotTh.Knotted.Threads
import Math.Topology.KnotTh.Moves.AdHoc
import Math.Topology.KnotTh.Moves.ModifyDSL
import Math.Topology.KnotTh.Tangle


class (Eq a, Ord a, Num a) => KauffmanFArg a where
    twistFactor  :: Int -> a
    zeroFactor   :: a
    inftyFactor  :: a
    loopFactor   :: Int -> a
    swapTwists   :: a -> a


instance KauffmanFArg P.Poly2 where
    twistFactor p = P.monomial2 1 "a" (fromIntegral p)
    zeroFactor    = P.monomial2 1 "z" 1
    inftyFactor   = P.monomial2 (-1) "z" 1
    loopFactor n  = ((P.monomial2 1 "a" 1 - P.monomial2 1 "a" (-1)) * P.monomial2 1 "z" (-1) + 1) ^ n
    swapTwists    = P.invert2 "a"


data BirmanWenzlAlgebra a = BW {-# UNPACK #-} !Int !(Map.Map (UV.Vector Int) a)
     deriving (Eq, Ord, Functor)

instance (NFData a) => NFData (BirmanWenzlAlgebra a) where
    rnf (BW _ m) = rnf m

instance (Show a) => Show (BirmanWenzlAlgebra a) where
    show (BW _ m) =
        case Map.toList m of
            [] -> "0"
            l  -> intercalate "+" $ map (\ (cd, f) -> printf "(%s)%s" (show f) (show $ UV.toList cd)) l

instance (KauffmanFArg a) => RotationAction (BirmanWenzlAlgebra a) where
    rotationOrder (BW d _) = d

    rotateByUnchecked !rot (BW d m) =
        BW d $ Map.filter (/= 0) $ Map.unionsWith (+) $ do
            (cd, f) <- Map.toList m
            let BW _ m' = bruteForceDubrovnik f (rotateByUnchecked rot $ restoreBasicTangle cd)
            return $! m'

instance (KauffmanFArg a) => MirrorAction (BirmanWenzlAlgebra a) where
    mirrorIt (BW d m) =
        BW d $ Map.filter (/= 0) $ Map.unionsWith (+) $ do
            (cd, f) <- Map.toList m
            let BW _ m' = bruteForceDubrovnik f (mirrorIt $ restoreBasicTangle cd)
            return $! m'

instance (KauffmanFArg a) => TensorProduct (BirmanWenzlAlgebra a) where
    a ⊗ b = horizontalComposition 0 (a, 0) (b, 0)

instance (KauffmanFArg a) => PlanarAlgebra (BirmanWenzlAlgebra a) where
    planarDegree (BW d _) = d

    planarEmpty = BW 0 $ Map.singleton UV.empty 1

    planarLoop n = BW 0 $ Map.singleton UV.empty (loopFactor n)

    planarPropagator n | n < 0      = error $ printf "planarPropagator: parameter must be non-negative, but %i passed" n
                       | otherwise  = BW (2 * n) $ Map.singleton (UV.generate (2 * n) $ \ i -> 2 * n - 1 - i) 1

    horizontalCompositionUnchecked gl (BW dA mA, !posA) (BW dB mB, !posB) =
        BW (dA + dB - 2 * gl) $ Map.filter (/= 0) $ Map.unionsWith (+) $ do
            (cdA, fA) <- Map.toList mA
            (cdB, fB) <- Map.toList mB
            let BW _ m = bruteForceDubrovnik (fA * fB) $ horizontalCompositionUnchecked gl (restoreBasicTangle cdA, posA)
                                                                                           (restoreBasicTangle cdB, posB)
            return $! m

instance (KauffmanFArg a) => TransposeAction (BirmanWenzlAlgebra a) where
    transposeIt = fmap swapTwists

instance (KauffmanFArg a) => SkeinRelation BirmanWenzlAlgebra a where
    crossingSkein OverCrossing = overBW
    crossingSkein UnderCrossing = underBW


overBW, underBW :: (KauffmanFArg a) => BirmanWenzlAlgebra a
overBW  = bruteForceDubrovnik 1 (toTangle lonerOverCrossing)
underBW = bruteForceDubrovnik 1 (toTangle lonerUnderCrossing)


{-# INLINE haveIntersection #-}
haveIntersection :: (Int, Int) -> (Int, Int) -> Bool
haveIntersection (!a', !b') (!c', !d') =
    let a = min a' b' ; b = max a' b'
        c = min c' d' ; d = max c' d'
    in (a < c && b > c && b < d) || (a > c && a < d && b > d)


{-# INLINE canonicalOver #-}
canonicalOver :: Int -> (Int, Int) -> (Int, Int) -> Bool
canonicalOver _ (!a, !b) (!c, !d) =
    min a b < min c d
--    let sa = min (abs $ a - b) $ n - abs (a - b)
--        sb = min (abs $ c - d) $ n - abs (c - d)
--    in (sa, min a b) < (sb, min c d)


restoreBasicTangle :: UV.Vector Int -> TangleDiagram
restoreBasicTangle !chordDiagram =
    let cdl = UV.length chordDiagram

        restore _ _ [] = error "restoreBasicTangle: impossible happened"
        restore a h (i : rest) =
            if | l == 0                           -> planarEmpty
               | l == 2                           -> planarPropagator 1
               | i' == j                          ->
                   let tangle = restore (UV.generate (l - 2) (\ x -> ((a UV.! ((i + 2 + x) `mod` l)) - i - 2) `mod` l))
                                        (UV.generate (l - 2) $ \ x -> h UV.! ((i + 2 + x) `mod` l))
                                        [0 .. l - 3]
                   in rotateBy i $ horizontalComposition 0 (planarPropagator 1, 0) (tangle, 0)
               | haveIntersection (i, i') (j, j') ->
                   let tangle = restore (a UV.// [(i, j'), (j, i'), (i', j), (j', i)]) (h UV.// [(i, h UV.! j), (j, h UV.! i)]) [0 .. l - 1]
                   in rotateBy i $ vertexOwner $ glueToBorder 2 (tangle, j) $
                       overCrossingIf $ canonicalOver cdl (h UV.! i) (h UV.! j)
               | otherwise                        -> restore a h rest
            where
                l = UV.length a
                i' = a UV.! i
                j = (i + 1) `mod` l
                j' = a UV.! j

    in if | cdl == 0  -> planarEmpty
          | otherwise -> restore chordDiagram (UV.generate cdl $ \ i -> (i, chordDiagram UV.! i)) [0 .. cdl - 1]


data ThreadTag = BorderThread {-# UNPACK #-} !(Int, Int) {-# UNPACK #-} !Int
               | InternalThread {-# UNPACK #-} !Int {-# UNPACK #-} !Int


bruteForceDubrovnik :: (KauffmanFArg a) => a -> TangleDiagram -> BirmanWenzlAlgebra a
bruteForceDubrovnik =
    let decompose path (!factor, !tangle) =
            let legs = numberOfLegs tangle

                (tn, _, threads) = allThreadsWithMarks tangle

                expectedPassOver =
                    let tags :: A.Array TangleDiagramDart ThreadTag
                        tags = A.array (dartsRange tangle) $ do
                            (tid, thread) <- threads
                            let make = case thread of
                                    []                     -> error "internal error"
                                    (h, _) : _ | isDart h  -> InternalThread $ numberOfLegs tangle + tid
                                               | otherwise -> let a = legPlace h
                                                                  b = legPlace $ snd $ last thread
                                                              in BorderThread (min a b, max a b)
                            (ord, (a, b)) <- [0 ..] `zip` thread
                            [(a, make $ 2 * ord), (b, make $ 2 * ord + 1)]

                        tagPassOver (InternalThread a ai) (InternalThread b bi) = (a, ai) < (b, bi)
                        tagPassOver (InternalThread _ _)  (BorderThread _ _)    = False
                        tagPassOver (BorderThread _ _)    (InternalThread _ _)  = True
                        tagPassOver (BorderThread a ai)   (BorderThread b bi)
                            | a == b                = ai < bi
                            | haveIntersection a b  = canonicalOver legs a b
                            | otherwise             = a < b

                    in \ d -> on tagPassOver (tags A.!) d (nextCCW d)

                irregs = filter (\ c -> let d0 = nthOutcomingDart c 0 in isPassingOver d0 /= expectedPassOver d0) $ allVertices tangle

                base =
                    let chordd = (UV.replicate legs 0 UV.//) $ do
                            (_, thread) <- threads
                            case thread of
                                (h, _) : _ | isLeg h ->
                                    let i = legPlace $ fst $ head thread
                                        j = legPlace $ snd $ last thread
                                    in [(i, j), (j, i)]
                                _                    -> []

                    in BW legs $ Map.singleton chordd $
                       let w = totalSelfWrithe' $ modifyKnot tangle $ modifyC False transposeIt irregs
                       in factor * twistFactor w * loopFactor (tn - legs `div` 2)

                splices [] inter _ =
                    -- (if length path >= 10 then trace (show $ map explode $ tangle : path) else id) $
                        base : map (decompose (tangle : path)) inter
                splices (h : rest) inter toInvert =
                    let a = ( zeroFactor * factor, modifyKnot tangle $ modifyC False transposeIt toInvert >>  zeroSmoothing h >> greedy [reduce2nd])
                        b = (inftyFactor * factor, modifyKnot tangle $ modifyC False transposeIt toInvert >> inftySmoothing h >> greedy [reduce2nd])
                    in splices rest (a : b : inter) (h : toInvert)

            in BW legs $ Map.filter (/= 0) $ Map.unionsWith (+) $ do
                BW _ m <- splices irregs [] []
                return $! m

    in curry (decompose [])


class (Knotted k) => KnottedWithDubrovnikPolynomial k where
    type DubrovnikPolynomial k :: *
    dubrovnikPolynomial        :: k DiagramCrossing -> DubrovnikPolynomial k
    minimalDubrovnikPolynomial :: k DiagramCrossing -> DubrovnikPolynomial k


instance KnottedWithDubrovnikPolynomial Tangle where
    type DubrovnikPolynomial Tangle = BirmanWenzlAlgebra P.Poly2

    dubrovnikPolynomial tangle =
        let factor = twistFactor (-totalSelfWrithe' tangle) * loopFactor (numberOfFreeLoops tangle)
        in (factor *) `fmap` reduceSkein tangle

    minimalDubrovnikPolynomial = skeinRelationPreMinimization dubrovnikPolynomial


instance KnottedWithDubrovnikPolynomial Tangle0 where
    type DubrovnikPolynomial Tangle0 = P.Poly2

    dubrovnikPolynomial link =
        let BW 0 m = dubrovnikPolynomial (toTangle link)
        in Map.findWithDefault 0 UV.empty m

    minimalDubrovnikPolynomial link =
        let f = kauffmanFPolynomial link
        in min f (swapTwists f)


kauffmanFPolynomial :: LinkDiagram -> P.Poly2
kauffmanFPolynomial link =
    P.dubrovnikToKauffmanF (totalCrossWrithe' link) (numberOfThreads link) $
        dubrovnikPolynomial link


normalizedKauffmanFPolynomialOfLink :: LinkDiagram -> P.Poly2
normalizedKauffmanFPolynomialOfLink link | isEmpty    = error "normalizedKauffmanFPolynomialOfLink: empty link provided"
                                         | otherwise  = P.normalizeBy2 (common * den) (common * num)
    where isEmpty = numberOfVertices link == 0 && numberOfFreeLoops link == 0
          num = kauffmanFPolynomial link
          den = (P.monomial2 1 "a" 1 + P.monomial2 1 "a" (-1)) * P.monomial2 1 "z" (-1) - 1
          common = P.monomial2 1 "a" 1 * P.monomial2 1 "z" 1
