{-# LANGUAGE DataKinds, GADTs, KindSignatures, ScopedTypeVariables #-}
module Math.Topology.KnotTh.Algebra.PlanarAlgebra.CableAlgebra
    ( CableAlgebra
    , packCable
    , unpackCable
    ) where

import GHC.TypeLits
import Data.Proxy
import Text.Printf
import Math.Topology.KnotTh.Algebra
import Math.Topology.KnotTh.Algebra.Dihedral
import Math.Topology.KnotTh.Algebra.PlanarAlgebra


newtype CableAlgebra :: Nat -> * -> * where
    Cab :: a -> CableAlgebra n a

instance (KnownNat n, RotationAction a) => RotationAction (CableAlgebra n a) where
    rotationOrder (Cab x) =
        let c = fromIntegral $ natVal (Proxy :: Proxy n)
        in rotationOrder x `div` c

    rotateByUnchecked rot (Cab x) =
        let c = fromIntegral $ natVal (Proxy :: Proxy n)
        in Cab $ rotateByUnchecked (rot * c) x

instance (KnownNat n, TensorProduct a) => TensorProduct (CableAlgebra n a) where
    Cab a ⊗ Cab b = Cab (a ⊗ b)

instance (KnownNat n, PlanarAlgebra a) => PlanarAlgebra (CableAlgebra n a) where
    planarDegree (Cab x) =
        let c = fromIntegral $ natVal (Proxy :: Proxy n)
        in rotationOrder x `div` c

    planarEmpty =
        Cab planarEmpty

    planarLoop l =
        let c = fromIntegral $ natVal (Proxy :: Proxy n)
        in Cab $ planarLoop (l * c)

    planarPropagator l =
        let c = fromIntegral $ natVal (Proxy :: Proxy n)
        in Cab $ planarPropagator (l * c)

    horizontalCompositionUnchecked gl (Cab a, posA) (Cab b, posB) =
        let c = fromIntegral $ natVal (Proxy :: Proxy n)
        in Cab $ horizontalCompositionUnchecked (gl * c) (a, posA * c) (b, (posB + 1) * c - 1)

    horizontalLooping gl (Cab a, pos) =
        let c = fromIntegral $ natVal (Proxy :: Proxy n)
        in Cab $ horizontalLooping (gl * c) (a, pos * c)


packCable :: forall n a. (KnownNat n, PlanarAlgebra a) => a -> CableAlgebra n a
packCable x | d `mod` c == 0  = Cab x
            | otherwise       = error $ printf "packCable: planarDegree %i is not divisible by cabling %i" d c
    where d = planarDegree x
          c = fromIntegral $ natVal (Proxy :: Proxy n)


unpackCable :: CableAlgebra n a -> a
unpackCable (Cab x) = x
