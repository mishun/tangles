{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Cobordism
    ( Cobordism(..)
    , Cobordism3(..)
    , PreadditiveCobordism(..)
    , CannedCobordism(..)
    ) where

import Math.Topology.KnotTh.PlanarAlgebra


class (Eq c, Eq (CobordismBorder c)) => Cobordism c where
    data CobordismBorder c :: *

    cobordismBorder0  :: c -> CobordismBorder c
    cobordismBorder1  :: c -> CobordismBorder c
    identityCobordism :: CobordismBorder c -> c
    flipCobordism     :: c -> c
    (∘)               :: c -> c -> c
    (⊗)               :: c -> c -> c

class (Cobordism c) => Cobordism3 c where
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
    capOfGenusCobordism = flipCobordism . cupOfGenusCobordism
    cupOfGenusCobordism = flipCobordism . capOfGenusCobordism
    pantsCobordism'     = flipCobordism pantsCobordism

class (Cobordism c, Num c) => PreadditiveCobordism c where
    zeroCobordism :: CobordismBorder c -> CobordismBorder c -> c

class (Cobordism3 c, PlanarAlgebra' c, PlanarAlgebra' (CobordismBorder c)) => CannedCobordism c where
    saddleCobordism        :: c
    saddleCobordism'       :: c
    sideCutPantsCobordism  :: c
    sideCutPantsCobordism' :: c

    saddleCobordism'       = flipCobordism saddleCobordism
    sideCutPantsCobordism  = horizontalComposition 2 (saddleCobordism', 0) (planarPropagator 1, 0)
    sideCutPantsCobordism' = horizontalComposition 2 (saddleCobordism, 0) (planarPropagator 1, 0)
