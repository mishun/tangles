{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Cobordism
    ( module Math.Topology.KnotTh.Dihedral
    , Cobordism(..)
    , Cobordism3(..)
    , PreadditiveCobordism(..)
    , CannedCobordism(..)
    ) where

import Math.Topology.KnotTh.ChordDiagram
import Math.Topology.KnotTh.Dihedral
import Math.Topology.KnotTh.PlanarAlgebra


class (Composition c, TensorProduct c, TensorProduct (CobordismBorder c), Eq (CobordismBorder c)) => Cobordism c where
    data CobordismBorder c :: *

    cobordismBorder0  :: c -> CobordismBorder c
    cobordismBorder1  :: c -> CobordismBorder c
    identityCobordism :: CobordismBorder c -> c

class (Cobordism c) => Cobordism3 c where
    transposeCobordism   :: c -> c
    numberOfLoops        :: CobordismBorder c -> Int
    surfOfGenusCobordism :: Int -> c
    sphereCobordism      :: c
    torusCobordism       :: c
    capCobordism         :: c
    cupCobordism         :: c
    capOfGenusCobordism  :: Int -> c
    cupOfGenusCobordism  :: Int -> c
    tubeCobordism        :: c
    swapCobordism        :: c
    pantsCobordism       :: c
    pantsCobordism'      :: c

    sphereCobordism     = surfOfGenusCobordism 0
    torusCobordism      = surfOfGenusCobordism 1
    capCobordism        = capOfGenusCobordism 0
    cupCobordism        = cupOfGenusCobordism 0
    capOfGenusCobordism = transposeCobordism . cupOfGenusCobordism
    cupOfGenusCobordism = transposeCobordism . capOfGenusCobordism
    pantsCobordism'     = transposeCobordism pantsCobordism

class (Cobordism c, Eq c, Num c) => PreadditiveCobordism c where
    zeroCobordism   :: CobordismBorder c -> CobordismBorder c -> c
    isZeroCobordism :: c -> Bool

    isZeroCobordism c = c == zeroCobordism (cobordismBorder0 c) (cobordismBorder1 c)

class (Cobordism3 c, PlanarAlgebra c, PlanarAlgebra (CobordismBorder c), ChordDiagram (CobordismBorder c)) => CannedCobordism c where
    saddleCobordism        :: c
    saddleCobordism'       :: c
    sideCutPantsCobordism  :: c
    sideCutPantsCobordism' :: c

    saddleCobordism'       = transposeCobordism saddleCobordism
    sideCutPantsCobordism  = horizontalComposition 2 (saddleCobordism', 0) (planarPropagator 1, 0)
    sideCutPantsCobordism' = horizontalComposition 2 (saddleCobordism, 0) (planarPropagator 1, 0)
