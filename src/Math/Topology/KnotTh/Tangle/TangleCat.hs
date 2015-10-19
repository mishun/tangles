{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Tangle.TangleCat
    ( TangleCategory
    , promoteTangle
    , promoteTangle0
    , promoteTangle1
    , promoteTangleH
    , identityBraid
    , braid
    , reversingBraid
    ) where

import Text.Printf
import Math.Topology.KnotTh.Algebra.Cobordism
import Math.Topology.KnotTh.Algebra.PlanarAlgebra
import Math.Topology.KnotTh.Tangle.TangleDef


data TangleCategory a = TC {-# UNPACK #-} !Int {-# UNPACK #-} !Int !(Tangle a)

instance Composition (TangleCategory a) where
    TC n10 n11 t1 ∘ TC n00 n01 t0 | n01 /= n10  = error $ printf "(∘): different numbers of legs (%i and %i)" n01 n10
                                  | otherwise   = TC n00 n11 $ horizontalComposition n01 (t0, n00) (t1, 0)

instance TensorProduct (CobordismBorder (TangleCategory a)) where
    B a ⊗ B b = B (a + b)

instance TensorProduct (TangleCategory a) where
    TC nA0 nA1 a ⊗ TC nB0 nB1 b =
        TC (nA0 + nB0) (nA1 + nB1) $ rotateBy (-nA1) $ horizontalComposition 0 (a, nA0) (b, 0)

instance Cobordism (TangleCategory a) where
    newtype CobordismBorder (TangleCategory a) = B Int
        deriving (Eq)

    cobordismBorder0 (TC n _ _) = B n
    cobordismBorder1 (TC _ n _) = B n

    identityCobordism (B n) = identityBraid n

instance (MirrorAction a) => MirrorAction (TangleCategory a) where
    mirrorIt (TC n0 n1 t) = TC n1 n0 $ rotateBy (-1) $ mirrorIt t

instance AsTangle TangleCategory where
    toTangle (TC _ _ t) = t


promoteTangle :: Int -> Int -> Tangle a -> TangleCategory a
promoteTangle n0 n1 t | n0 < 0 || n1 < 0  = error "promoteTangle: border sizes (%i and %i) must be non-negative" n0 n1
                      | n0 + n1 /= l      = error "promoteTangle: border sizes are %i and %i, but number of legs is %i" n0 n1 l
                      | otherwise         = TC n0 n1 t
    where l = numberOfLegs t


promoteTangle0 :: Tangle a -> TangleCategory a
promoteTangle0 t = TC (numberOfLegs t) 0 t


promoteTangle1 :: Tangle a -> TangleCategory a
promoteTangle1 t = TC 0 (numberOfLegs t) t


promoteTangleH :: Tangle a -> TangleCategory a
promoteTangleH t =
    let l = numberOfLegs t
    in TC (l `div` 2) (l `div` 2) t


identityBraid :: Int -> TangleCategory a
identityBraid n = TC n n (planarPropagator n)


braidGenerator :: Int -> (Int, a) -> TangleCategory a
braidGenerator n (k, s) | n < 2               = error $ printf "braidGenerator: braid must have at least 2 strands, but %i requested" n
                        | k < 0 || k > n - 2  = error $ printf "braidGenerator: generator offset %i is out of bounds (0, %i)" k (n - 2)
                        | otherwise           = identityBraid k ⊗ promoteTangleH (toTangle $ lonerTangle s) ⊗ identityBraid (n - 2 - k)


braid :: Int -> [(Int, a)] -> TangleCategory a
braid n = foldl (∘) (identityBraid n) . map (braidGenerator n)


reversingBraid :: Int -> a -> TangleCategory a
reversingBraid n s | n < 0      = error $ printf "reversingBraid: requested number of strands %i is negative" n
                   | otherwise  = braid n [ (i, s) | k <- [2 .. n], i <- [0 .. n - k] ]
