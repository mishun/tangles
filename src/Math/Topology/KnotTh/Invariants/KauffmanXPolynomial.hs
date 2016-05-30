{-# LANGUAGE DeriveFunctor, FlexibleInstances, MultiParamTypeClasses, MultiWayIf, TypeFamilies #-}
module Math.Topology.KnotTh.Invariants.KauffmanXPolynomial
    ( TemperleyLiebAlgebra
    , kauffmanXPolynomial
    , minimalKauffmanXPolynomial
    , jonesPolynomial
    , minimalJonesPolynomial
    , normalizedJonesPolynomialOfLink
    ) where

import Control.Arrow (first)
import Control.Monad (foldM, forM_, liftM2, when)
import Control.Monad.IfElse (unlessM)
import qualified Control.Monad.ST as ST
import Data.List (intercalate, partition)
import qualified Data.Map.Strict as Map
import Data.Function (fix)
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Text.Printf
import Math.Topology.KnotTh.Algebra
import Math.Topology.KnotTh.Invariants.Util.Poly
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.KnotPolynomials.Surface
import Math.Topology.KnotTh.Invariants.LinkingNumbers
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.SurfaceGraph.Homology
import Math.Topology.KnotTh.Tangle


class (Eq a, Ord a, Num a) => KauffmanXArg a where
    aFactor, bFactor :: a
    transposeFactors :: a -> a


loopFactor :: (KauffmanXArg a) => a
loopFactor = -(aFactor * aFactor + bFactor * bFactor)


instance KauffmanXArg Poly where
    aFactor = monomial 1 "a" 1
    bFactor = monomial 1 "a" (-1)
    transposeFactors = invert "a"


data TemperleyLiebAlgebra a = TL !Int !(Map.Map (UV.Vector Int) a)
    deriving (Eq, Ord, Functor)

instance (Show a) => Show (TemperleyLiebAlgebra a) where
    show (TL _ m) =
        case Map.toList m of
            [] -> "0"
            l  -> intercalate "+" $ map (\ (cd, f) -> printf "(%s)%s" (show f) (show $ UV.toList cd)) l

instance (KauffmanXArg a) => RotationAction (TemperleyLiebAlgebra a) where
    rotationOrder (TL d _) = d

    rotateByUnchecked !rot (TL l m) =
        let rotate x = UV.create $ do
                a <- UMV.new l
                forM_ [0 .. l - 1] $ \ !i ->
                    UMV.write a ((i + rot) `mod` l) (((x UV.! i) + rot) `mod` l)
                return a
        in TL l (Map.mapKeys rotate m)

instance (KauffmanXArg a) => MirrorAction (TemperleyLiebAlgebra a) where
    mirrorIt (TL l m) =
        let mirror x = UV.create $ do
                a <- UMV.new l
                forM_ [0 .. l - 1] $ \ !i ->
                    UMV.write a ((-i) `mod` l) ((-(x UV.! i)) `mod` l)
                return a
        in TL l (Map.mapKeys mirror m)

instance (KauffmanXArg a) => TensorProduct (TemperleyLiebAlgebra a) where
    a ⊗ b = horizontalComposition 0 (a, 0) (b, 0)

instance (KauffmanXArg a) => PlanarAlgebra (TemperleyLiebAlgebra a) where
    planarDegree (TL d _) = d

    planarEmpty = TL 0 $ Map.singleton UV.empty 1

    planarLoop n = TL 0 $ Map.singleton UV.empty (loopFactor ^ n)

    planarPropagator n | n < 0      = error $ printf "planarPropagator: parameter must be non-negative, but %i passed" n
                       | otherwise  = TL (2 * n) $ Map.singleton (UV.generate (2 * n) $ \ i -> 2 * n - 1 - i) 1

    horizontalCompositionUnchecked !gl (TL legsA mapA, !posA) (TL legsB mapB, !posB) =
        TL (legsA + legsB - 2 * gl) $ Map.filter (/= 0) $ Map.fromListWith (+) $ do
            (a, factorA) <- Map.toList mapA
            (b, factorB) <- Map.toList mapB
            return $! ST.runST $ do
                visited <- UMV.replicate gl False

                arcs <-
                    let mateA !x | y >= gl    = return $! y - gl
                                 | otherwise  = do
                                     UMV.write visited y True
                                     mateB $ (posB + gl - 1 - y) `mod` legsB
                            where y = ((a UV.! x) - posA) `mod` legsA

                        mateB !x | y >= gl    = return $! legsA + y - 2 * gl
                                 | otherwise  = mateA $ (posA + gl - 1 - y) `mod` legsA
                            where y = ((b UV.! x) - posB) `mod` legsB

                    in liftM2 (UV.++) (UV.generateM (legsA - gl) (\ !i -> mateA $ (posA + gl + i) `mod` legsA))
                                      (UV.generateM (legsB - gl) (\ !i -> mateB $ (posB + gl + i) `mod` legsB))

                loops <-
                    let markA !x =
                            unlessM (UMV.read visited x) $ do
                                UMV.write visited x True
                                markB $ (`mod` legsA) $ (+ negate posA) $ (a UV.!) $ (posA + x) `mod` legsA

                        markB !x =
                            unlessM (UMV.read visited x) $ do
                                UMV.write visited x True
                                markA $ (`mod` legsB) $ (\ p -> posB - p + gl - 1) $ (b UV.!) $ (posB + gl - 1 - x) `mod` legsB

                    in foldM (\ !loops !i -> do
                            v <- UMV.read visited i
                            if v then return loops
                                 else do
                                     markA i
                                     return $! 1 + loops
                        ) (0 :: Int) [0 .. gl - 1]

                return (arcs, factorA * factorB * loopFactor ^ loops)

    horizontalLooping 1 (TL degree m, !p) =
        let !p' = (p + 1) `mod` degree

            subst = UV.create $ do
                a <- UMV.replicate degree (-1)
                forM_ [0 .. degree - 3] $ \ !i ->
                    UMV.write a ((p + 2 + i) `mod` degree) i
                return a

        in TL (degree - 2) $ Map.filter (/= 0) $ Map.fromListWith (+) $ do
            (x, k) <- Map.toList m
            let x' = UV.create $ do
                    xm <- UMV.new (degree - 2)
                    forM_ [0 .. degree - 1] $ \ !i ->
                        when (i /= p' && i /= p) $
                            let j | (x UV.! i) == p   = x UV.! p'
                                  | (x UV.! i) == p'  = x UV.! p
                                  | otherwise         = x UV.! i
                            in UMV.write xm (subst UV.! i) (subst UV.! j)
                    return xm
            return $! (,) x' $
                if | x UV.! p == p' -> k * loopFactor
                   | otherwise      -> k

    horizontalLooping n _ = error $ printf "KauffmanXStateSum.horizontalLooping: not implemented for %i" n

instance (KauffmanXArg a) => TransposeAction (TemperleyLiebAlgebra a) where
    transposeIt = fmap transposeFactors

instance (KauffmanXArg a) => SkeinRelation TemperleyLiebAlgebra a where
    crossingSkein OverCrossing =
        TL 4 $ Map.fromList
            [ (UV.fromList [3, 2, 1, 0], aFactor)
            , (UV.fromList [1, 0, 3, 2], bFactor)
            ]

    crossingSkein UnderCrossing =
        TL 4 $ Map.fromList
            [ (UV.fromList [3, 2, 1, 0], bFactor)
            , (UV.fromList [1, 0, 3, 2], aFactor)
            ]


class (Knotted k) => KnottedWithKauffmanXPolynomial k where
    type KauffmanXPolynomial k :: *
    kauffmanXPolynomial        :: k DiagramCrossing -> KauffmanXPolynomial k
    minimalKauffmanXPolynomial :: k DiagramCrossing -> KauffmanXPolynomial k


instance KnottedWithKauffmanXPolynomial Tangle where
    type KauffmanXPolynomial Tangle = TemperleyLiebAlgebra Poly

    kauffmanXPolynomial tangle =
        let factor =
                let writheFactor =
                        let w = totalSelfWrithe' tangle
                        in (if w <= 0 then -aFactor else -bFactor) ^ abs (3 * w)
                    loopsFactor = loopFactor ^ numberOfFreeLoops tangle
                in writheFactor * loopsFactor
        in (factor *) `fmap` reduceSkein tangle

    minimalKauffmanXPolynomial = skeinRelationPostMinimization kauffmanXPolynomial


instance KnottedWithKauffmanXPolynomial Tangle0 where
    type KauffmanXPolynomial Tangle0 = Poly

    kauffmanXPolynomial link =
        let TL 0 m = kauffmanXPolynomial $ toTangle link
        in Map.findWithDefault 0 UV.empty m

    minimalKauffmanXPolynomial link =
        let x = kauffmanXPolynomial link
        in min x (transposeFactors x)


instance KnottedWithKauffmanXPolynomial EmbeddedLink where
    type KauffmanXPolynomial EmbeddedLink = Poly2

    kauffmanXPolynomial link | numberOfVertices link == 0  = 1
                             | otherwise                   =
        case eulerCharOf link of
            2 -> kauffmanXPolynomial (toLink link)

            0 -> sum $ do
                let (dim, weightedLoopSystems) = homologyDecomposition link

                    tab = Map.toList $ Map.filter (/= 0) $
                        Map.fromListWith (+) $ do
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
        min (kauffmanXPolynomial link)
            (kauffmanXPolynomial $ transposeCrossings link)


homologyDecomposition :: EmbeddedLinkDiagram -> (Int, [([UV.Vector Int], Poly)])
homologyDecomposition link =
    let (tangle, star) = splitIntoTangleAndStar link
        l = numberOfLegs tangle
        (dim, homology) = cellularHomology $ vertexOwner star

        homologyClasses a = ST.runST $ do
            visited <- UMV.replicate l False
            foldM (\ !list !start -> do
                    vs <- UMV.read visited start
                    if vs
                        then return list
                        else do
                            lp <- fix (\ loop hom !i -> do
                                    c <- UMV.read visited i
                                    if c
                                        then return hom
                                        else do
                                            let d = nthOutcomingDart star (l - 1 - i)
                                                i' = l - 1 - endPlace d
                                                hom' = homology d
                                            UMV.write visited i True
                                            UMV.write visited i' True
                                            loop (UV.zipWith (+) hom hom') (a UV.! i')
                                ) (UV.replicate dim 0) start
                            return $ max lp (UV.map negate lp) : list
                ) [] [0 .. l - 1]

        tokens =
            let TL _ m = kauffmanXPolynomial tangle
            in map (first homologyClasses) $ Map.toList m

    in (dim, tokens)


class (KnottedWithKauffmanXPolynomial k) => KnottedWithJonesPolynomial k where
    jonesPolynomial        :: k DiagramCrossing -> KauffmanXPolynomial k
    minimalJonesPolynomial :: k DiagramCrossing -> KauffmanXPolynomial k


instance KnottedWithJonesPolynomial Tangle where
    jonesPolynomial = fmap kauffmanXToJones . kauffmanXPolynomial
    minimalJonesPolynomial = fmap kauffmanXToJones . minimalKauffmanXPolynomial


instance KnottedWithJonesPolynomial Link where
    jonesPolynomial = kauffmanXToJones . kauffmanXPolynomial
    minimalJonesPolynomial = kauffmanXToJones . minimalKauffmanXPolynomial


normalizedJonesPolynomialOfLink :: LinkDiagram -> Poly
normalizedJonesPolynomialOfLink link | isEmpty    = error "normalizedJonesPolynomialOfLink: empty link provided"
                                     | otherwise  = normalizeBy (1 + monomial 1 "t" 1) (monomial (-1) "t" (1 / 2) * jonesPolynomial link)
    where isEmpty = numberOfVertices link == 0 && numberOfFreeLoops link == 0
