{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, MultiWayIf #-}
module Math.Topology.KnotTh.Invariants.KnotPolynomials.KauffmanXStateSum
    ( KauffmanXArg(..)
    , loopFactor
    , PlanarChordDiagram(..)
    , KauffmanXStateSum(..)
    ) where

import Control.Monad (foldM, forM_, liftM2, when)
import Control.Monad.IfElse (unlessM)
import qualified Control.Monad.ST as ST
import Data.List (foldl', intercalate)
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Text.Printf
import Math.Topology.KnotTh.Algebra
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Invariants.KnotPolynomials
import Math.Topology.KnotTh.Invariants.Util.Poly


class (Eq a, Ord a, Num a) => KauffmanXArg a where
    aFactor, bFactor :: a
    transposeFactors :: a -> a


loopFactor :: (KauffmanXArg a) => a
loopFactor = -(aFactor * aFactor + bFactor * bFactor)


instance KauffmanXArg Poly where
    aFactor = monomial 1 "a" 1
    bFactor = monomial 1 "a" (-1)
    transposeFactors = invert "a"


data PlanarChordDiagram a = PlanarChordDiagram !(UV.Vector Int) !a
    deriving (Eq, Ord)


instance Functor PlanarChordDiagram where
    fmap f (PlanarChordDiagram a x) =
        PlanarChordDiagram a (f x)


instance (Show a) => Show (PlanarChordDiagram a) where
    show (PlanarChordDiagram a x) =
        printf "(%s)%s" (show x) (show $ UV.toList a)


data KauffmanXStateSum a = KauffmanXStateSum !Int ![PlanarChordDiagram a]
    deriving (Eq, Ord)


instance Functor KauffmanXStateSum where
    fmap f (KauffmanXStateSum n l) =
        KauffmanXStateSum n (map (fmap f) l)

instance (Show a) => Show (KauffmanXStateSum a) where
    show (KauffmanXStateSum _ list) =
        case list of
            [] -> "0"
            _  -> intercalate "+" $ map show list


singletonStateSum :: PlanarChordDiagram a -> KauffmanXStateSum a
singletonStateSum summand @ (PlanarChordDiagram a _) =
    KauffmanXStateSum (UV.length a) [summand]


concatStateSums :: (Eq a, Num a) => [KauffmanXStateSum a] -> KauffmanXStateSum a
concatStateSums [] = error $ printf "concatStateSum: empty"
concatStateSums list @ (KauffmanXStateSum order _ : _) =
    KauffmanXStateSum order $ map (\ (!k, !v) -> PlanarChordDiagram k v) $
        filter ((/= 0) . snd) $ M.toList $
            foldl' (\ !m (PlanarChordDiagram !k !v) -> M.insertWith' (+) k v m) M.empty $
                concatMap (\ (KauffmanXStateSum order' list') ->
                        if order' == order
                            then list'
                            else error $ printf "concatStateSums: order conflict with %i and %i" order order'
                    ) list


mapStateSum :: (Eq a, Num a) => (PlanarChordDiagram a -> KauffmanXStateSum a) -> KauffmanXStateSum a -> KauffmanXStateSum a
mapStateSum _ (KauffmanXStateSum order []) = KauffmanXStateSum order []
mapStateSum f (KauffmanXStateSum _ list) = concatStateSums $ map f list


instance (KauffmanXArg a) => RotationAction (KauffmanXStateSum a) where
    rotationOrder (KauffmanXStateSum d _) = d

    rotateByUnchecked !rot =
        mapStateSum $ \ (PlanarChordDiagram x f) ->
            let x' = UV.create $ do
                    let l = UV.length x
                    a <- UMV.new l
                    forM_ [0 .. l - 1] $ \ !i ->
                        UMV.write a ((i + rot) `mod` l) (((x UV.! i) + rot) `mod` l)
                    return a
            in singletonStateSum $ PlanarChordDiagram x' f

instance (KauffmanXArg a) => MirrorAction (KauffmanXStateSum a) where
    mirrorIt =
        mapStateSum $ \ (PlanarChordDiagram x f) ->
            let x' = UV.create $ do
                    let l = UV.length x
                    a <- UMV.new l
                    forM_ [0 .. l - 1] $ \ !i ->
                        UMV.write a ((-i) `mod` l) ((-(x UV.! i)) `mod` l)
                    return a
            in singletonStateSum $ PlanarChordDiagram x' f

instance (KauffmanXArg a) => TensorProduct (KauffmanXStateSum a) where
    a ⊗ b = horizontalComposition 0 (a, 0) (b, 0)

instance (KauffmanXArg a) => PlanarAlgebra (KauffmanXStateSum a) where
    planarDegree (KauffmanXStateSum d _) = d

    planarEmpty = KauffmanXStateSum 0 [PlanarChordDiagram UV.empty 1]

    planarLoop n = KauffmanXStateSum 0 [PlanarChordDiagram UV.empty (loopFactor ^ n)]

    planarPropagator n | n < 0      = error $ printf "planarPropagator: parameter must be non-negative, but %i passed" n
                       | otherwise  = KauffmanXStateSum (2 * n) [PlanarChordDiagram (UV.generate (2 * n) $ \ i -> 2 * n - 1 - i) 1]

    horizontalCompositionUnchecked !gl (sumA@(KauffmanXStateSum !legsA _), !posA) (sumB@(KauffmanXStateSum !legsB _), !posB) =
        flip mapStateSum sumA $ \ (PlanarChordDiagram a factorA) ->
            flip mapStateSum sumB $ \ (PlanarChordDiagram b factorB) ->
                ST.runST $ do
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

                    return $! singletonStateSum (PlanarChordDiagram arcs (factorA * factorB * loopFactor ^ loops))

    horizontalLooping 1 (preSum@(KauffmanXStateSum !degree _), !p) =
        let !p' = (p + 1) `mod` degree

            subst = UV.create $ do
                a <- UMV.replicate degree (-1)
                forM_ [0 .. degree - 3] $ \ !i ->
                    UMV.write a ((p + 2 + i) `mod` degree) i
                return a

        in flip mapStateSum preSum $ \ (PlanarChordDiagram x k) ->
                let x' = UV.create $ do
                        xm <- UMV.new (degree - 2)
                        forM_ [0 .. degree - 1] $ \ !i ->
                            when (i /= p' && i /= p) $
                                let j | (x UV.! i) == p   = x UV.! p'
                                      | (x UV.! i) == p'  = x UV.! p
                                      | otherwise         = x UV.! i
                                in UMV.write xm (subst UV.! i) (subst UV.! j)
                        return xm
                in singletonStateSum $ PlanarChordDiagram x' $
                    if | x UV.! p == p' -> k * loopFactor
                       | otherwise      -> k
    horizontalLooping n _ = error $ printf "KauffmanXStateSum.horizontalLooping: not implemented for %i" n

instance (KauffmanXArg a) => TransposeAction (KauffmanXStateSum a) where
    transposeIt = fmap transposeFactors

instance (KauffmanXArg a) => SkeinRelation KauffmanXStateSum a where
    skeinLPlus =
        concatStateSums $ map singletonStateSum
            [ PlanarChordDiagram (UV.fromList [3, 2, 1, 0]) aFactor
            , PlanarChordDiagram (UV.fromList [1, 0, 3, 2]) bFactor
            ]

    skeinLMinus =
        concatStateSums $ map singletonStateSum
            [ PlanarChordDiagram (UV.fromList [3, 2, 1, 0]) bFactor
            , PlanarChordDiagram (UV.fromList [1, 0, 3, 2]) aFactor
            ]

    takeAsScalar s =
        case s of
            KauffmanXStateSum 0 []                       -> 0
            KauffmanXStateSum 0 [PlanarChordDiagram _ x] -> x
            _                                            -> undefined
