{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, MultiWayIf, TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.KauffmanFPolynomial
    ( BirmanWenzlAlgebra
    , kauffmanFPolynomial
    , minimalKauffmanFPolynomial
    , normalizedKauffmanFPolynomialOfLink
    ) where

import Control.DeepSeq
import qualified Data.Array as A
import Data.Function (on)
import Data.List (intercalate)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import Text.Printf
import Math.Topology.KnotTh.Algebra
import Math.Topology.KnotTh.Invariants.Util.Poly
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.LinkingNumbers
import Math.Topology.KnotTh.Knotted.Threads
import Math.Topology.KnotTh.Moves.AdHoc
import Math.Topology.KnotTh.Moves.ModifyDSL
import Math.Topology.KnotTh.Tangle


class (Eq a, Ord a, Num a) => KauffmanFArg a where
    twistFactor  :: Int -> a
    smoothFactor :: a
    loopFactor   :: Int -> a
    swapTwists   :: a -> a


instance KauffmanFArg Poly2 where
    twistFactor p = monomial2 1 "a" (fromIntegral p)
    smoothFactor  = monomial2 1 "z" 1
    loopFactor n  = ((twistFactor 1 + twistFactor (-1)) * monomial2 1 "z" (-1) - 1) ^ n
    swapTwists    = invert2 "a"


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
            let BW _ m' = bruteForceF f (rotateByUnchecked rot $ restoreBasicTangle cd)
            return $! m'

instance (KauffmanFArg a) => MirrorAction (BirmanWenzlAlgebra a) where
    mirrorIt (BW d m) =
        BW d $ Map.filter (/= 0) $ Map.unionsWith (+) $ do
            (cd, f) <- Map.toList m
            let BW _ m' = bruteForceF f (mirrorIt $ restoreBasicTangle cd)
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
            let BW _ m = bruteForceF (fA * fB) $ horizontalCompositionUnchecked gl (restoreBasicTangle cdA, posA) (restoreBasicTangle cdB, posB)
            return $! m

instance (KauffmanFArg a) => TransposeAction (BirmanWenzlAlgebra a) where
    transposeIt = fmap swapTwists

instance (KauffmanFArg a) => SkeinRelation BirmanWenzlAlgebra a where
    crossingSkein OverCrossing =
        BW 4 $ Map.singleton (UV.fromList [2, 3, 0, 1]) 1

    crossingSkein UnderCrossing =
        BW 4 $ Map.fromList
            [ (UV.fromList [2, 3, 0, 1], -1)
            , (UV.fromList [3, 2, 1, 0], smoothFactor)
            , (UV.fromList [1, 0, 3, 2], smoothFactor)
            ]


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

        restore :: UV.Vector Int -> V.Vector (Int, Int) -> [Int] -> TangleDiagram
        restore _ _ [] = error "restoreBasicTangle: impossible happened"
        restore a h (i : rest) =
            if | l == 0                           -> planarEmpty
               | l == 2                           -> planarPropagator 1
               | i' == j                          ->
                   let tangle = restore (UV.generate (l - 2) (\ x -> ((a UV.! ((i + 2 + x) `mod` l)) - i - 2) `mod` l))
                                        (V.generate (l - 2) $ \ x -> h V.! ((i + 2 + x) `mod` l))
                                        [0 .. l - 3]
                   in rotateBy i $ horizontalComposition 0 (planarPropagator 1, 0) (tangle, 0)
               | haveIntersection (i, i') (j, j') ->
                   let tangle = restore (a UV.// [(i, j'), (j, i'), (i', j), (j', i)]) (h V.// [(i, h V.! j), (j, h V.! i)]) [0 .. l - 1]
                   in rotateBy i $ vertexOwner $ glueToBorder 2 (tangle, j) $
                       overCrossingIf $ canonicalOver cdl (h V.! i) (h V.! j)
               | otherwise                        -> restore a h rest
            where
                l = UV.length a
                i' = a UV.! i
                j = (i + 1) `mod` l
                j' = a UV.! j

    in if | cdl == 0  -> planarEmpty
          | otherwise -> restore chordDiagram (V.generate cdl $ \ i -> (i, chordDiagram UV.! i)) [0 .. cdl - 1]


data ThreadTag = BorderThread {-# UNPACK #-} !(Int, Int) {-# UNPACK #-} !Int
               | InternalThread {-# UNPACK #-} !Int {-# UNPACK #-} !Int


bruteForceF :: (KauffmanFArg a) => a -> TangleDiagram -> BirmanWenzlAlgebra a
bruteForceF =
    let irregularCrossings tangle =
            let (_, _, threads) = allThreadsWithMarks tangle

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

                        tagPassOver _ (InternalThread a ai) (InternalThread b bi) = (a, ai) < (b, bi)
                        tagPassOver _ (InternalThread _ _)  (BorderThread _ _)    = False
                        tagPassOver _ (BorderThread _ _)    (InternalThread _ _)  = True
                        tagPassOver n (BorderThread a ai)   (BorderThread b bi)
                            | a == b                = ai < bi
                            | haveIntersection a b  = canonicalOver n a b
                            | otherwise             = a < b

                    in \ d -> on (tagPassOver $ numberOfLegs tangle) (tags A.!) d (nextCCW d)

            in filter (\ c ->
                    let d0 = nthOutcomingDart c 0
                    in isPassingOver d0 /= expectedPassOver d0
               ) $ allVertices tangle

        decompose path (!initialFactor, !tangle0) =
            let legs = numberOfLegs tangle0

                splices [] toInvert factor inter =
                    let normal = modifyKnot tangle0 $ modifyC False transposeIt toInvert

                        (n, _, threads) = allThreadsWithMarks normal

                        base =
                            let cd = (UV.replicate legs 0 UV.//) $ do
                                        (_, thread) <- threads
                                        case thread of
                                            (h, _) : _ | isLeg h ->
                                                let i = legPlace $ fst $ head thread
                                                    j = legPlace $ snd $ last thread
                                                in [(i, j), (j, i)]
                                            _                    -> []

                            in BW legs $ Map.singleton cd $
                               let w = totalSelfWrithe' normal
                               in factor * twistFactor w * loopFactor (n - legs `div` 2)

                    in {- (if length path >= 10 then trace (show $ map explode $ tangle0 : path) else id) $ -}
                        base : map (decompose (tangle0 : path)) inter

                splices (h : r) toInvert factor inter =
                    let a = (factor * smoothFactor, modifyKnot tangle0 $ modifyC False transposeIt toInvert >> smoothA h >> greedy [reduce2nd])
                        b = (factor * smoothFactor, modifyKnot tangle0 $ modifyC False transposeIt toInvert >> smoothB h >> greedy [reduce2nd])
                    in splices r (h : toInvert) (-factor) (a : b : inter)

            in BW legs $ Map.filter (/= 0) $ Map.unionsWith (+) $ do
                BW _ m <- splices (irregularCrossings tangle0) [] initialFactor []
                return $! m

    in curry (decompose [])


class (Knotted k) => KnottedWithKauffmanFPolynomial k where
    type KauffmanFPolynomial k :: *
    kauffmanFPolynomial        :: k DiagramCrossing -> KauffmanFPolynomial k
    minimalKauffmanFPolynomial :: k DiagramCrossing -> KauffmanFPolynomial k


instance KnottedWithKauffmanFPolynomial Tangle where
    type KauffmanFPolynomial Tangle = BirmanWenzlAlgebra Poly2

    kauffmanFPolynomial tangle =
        let factor =
                let writheFactor = twistFactor (-totalSelfWrithe' tangle)
                    loopsFactor = loopFactor (numberOfFreeLoops tangle)
                in writheFactor * loopsFactor
        in (factor *) `fmap` reduceSkein tangle

    minimalKauffmanFPolynomial = skeinRelationPreMinimization kauffmanFPolynomial


instance KnottedWithKauffmanFPolynomial Tangle0 where
    type KauffmanFPolynomial Tangle0 = Poly2

    kauffmanFPolynomial link =
        let (BW 0 m) = kauffmanFPolynomial (toTangle link)
        in fromMaybe 0 (Map.lookup UV.empty m)

    minimalKauffmanFPolynomial link =
        let f = kauffmanFPolynomial link
        in min f (swapTwists f)


normalizedKauffmanFPolynomialOfLink :: LinkDiagram -> Poly2
normalizedKauffmanFPolynomialOfLink link | isEmpty    = error "normalizedKauffmanFPolynomialOfLink: empty link provided"
                                         | otherwise  = normalizeBy2 (common * loopFactor 1) (common * kauffmanFPolynomial link)
    where isEmpty = numberOfVertices link == 0 && numberOfFreeLoops link == 0
          common = twistFactor 1 * smoothFactor
