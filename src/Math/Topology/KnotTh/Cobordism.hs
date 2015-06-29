{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Cobordism
    ( Cobordism(..)
    , CannedCobordism(..)
    ) where

import Math.Topology.KnotTh.PlanarAlgebra


class (Eq c, Eq (CobordismBorder c)) => Cobordism c where
    data CobordismBorder c :: *

    cobordismBorder0       :: c -> CobordismBorder c
    cobordismBorder1       :: c -> CobordismBorder c
    numberOfLoops          :: CobordismBorder c -> Int
    (∘)                    :: c -> c -> c
    (⊗)                    :: c -> c -> c
    flipCobordism          :: c -> c
    identityCobordism      :: CobordismBorder c -> c
    closedSurfaceCobordism :: Int -> c
    sphereCobordism        :: c
    torusCobordism         :: c
    capCobordism           :: c
    cupCobordism           :: c
    capCobordism'          :: Int -> c
    cupCobordism'          :: Int -> c
    tubeCobordism          :: c
    pantsCobordism         :: c
    pantsCobordism'        :: c

    sphereCobordism = closedSurfaceCobordism 0
    torusCobordism  = closedSurfaceCobordism 1
    capCobordism    = capCobordism' 0
    cupCobordism    = cupCobordism' 0
    capCobordism'   = flipCobordism . cupCobordism'
    cupCobordism'   = flipCobordism . capCobordism'
    pantsCobordism' = flipCobordism pantsCobordism


class (Cobordism c, PlanarAlgebra' c, PlanarAlgebra' (CobordismBorder c)) => CannedCobordism c where
    saddleCobordism        :: c
    saddleCobordism'       :: c
    sideCutPantsCobordism  :: c
    sideCutPantsCobordism' :: c

    saddleCobordism'       = flipCobordism saddleCobordism
    sideCutPantsCobordism  = horizontalComposition 2 (saddleCobordism', 0) (planarPropagator 1, 0)
    sideCutPantsCobordism' = horizontalComposition 2 (saddleCobordism, 0) (planarPropagator 1, 0)
