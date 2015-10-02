{-# LANGUAGE StandaloneDeriving, UndecidableInstances #-}
module Math.Topology.KnotTh.Invariants.KhovanovHomology
    ( module Math.Topology.KnotTh.Cobordism.DottedCobordism
    , KhovanovComplex(..)
    , complexDim
    , testComplexBorders
    , overCrossingComplex
    , underCrossingComplex
    , khovanovComplex
    ) where

import qualified Data.Vector as V
import qualified Math.Topology.KnotTh.Cobordism.CobordismMatrix as M
import Math.Topology.KnotTh.Cobordism.DottedCobordism
import Math.Topology.KnotTh.PlanarAlgebra.Reduction
import Math.Topology.KnotTh.Tangle


data BoundedChain c = Chain !(V.Vector (M.CobordismMatrix c))
                    | Singl !(CobordismBorder c)

deriving instance (Cobordism c, Show c, Show (CobordismBorder c)) => Show (BoundedChain c)


chainSpan :: BoundedChain c -> Int
chainSpan (Chain b) = V.length b
chainSpan (Singl _) = 0


glueChains :: (CannedCobordism c, PreadditiveCobordism c) => Int -> (BoundedChain c, Int) -> (BoundedChain c, Int) -> BoundedChain c
glueChains gl (Singl a, posA) (Singl b, posB) = Singl $ horizontalComposition gl (a, posA) (b, posB)
glueChains gl (Singl a, posA) (Chain bc, posB) =
    let arrow = M.singleton $ identityCobordism a
    in Chain $ V.map (\ b -> horizontalComposition gl (arrow, posA) (b, posB)) bc
glueChains gl (Chain ac, posA) (Singl b, posB) =
    let arrow = M.singleton $ identityCobordism b
    in Chain $ V.map (\ a -> horizontalComposition gl (a, posA) (arrow, posB)) ac
glueChains gl (Chain a, posA) (Chain b, posB) =
    let spanA = V.length a
        spanB = V.length b

        width d = max 0 $ (min d spanA) - (max 0 $ d - spanB) + 1

        coords d w =
            let pa = w + max 0 (d - spanB)
            in (pa, d - pa)

        layerA d | d == spanA  = cobordismBorder1 $ a V.! (d - 1)
                 | otherwise   = cobordismBorder0 $ a V.! d

        layerB d | d == spanB  = cobordismBorder1 $ b V.! (d - 1)
                 | otherwise   = cobordismBorder0 $ b V.! d

    in Chain $ V.generate (spanA + spanB) $ \ d ->
        let cols = V.generate (width d) $ \ w ->
                    let (pa, pb) = coords d w
                    in horizontalComposition gl (layerA pa, posA) (layerB pb, posB)

            rows = V.generate (width $ d + 1) $ \ w ->
                    let (pa, pb) = coords (d + 1) w
                    in horizontalComposition gl (layerA pa, posA) (layerB pb, posB)

        in M.flatten $ M.generate cols rows $ \ row col ->
            let (pa0, pb0) = coords d col
                (pa1, pb1) = coords (d + 1) row
            in if | pb1 == pb0             -> horizontalComposition gl (a V.! pa0, posA) (identityCobordism $ layerB pb0, posB)
                  | pa1 == pa0 && even pa0 -> horizontalComposition gl (identityCobordism $ layerA pa0, posA) (b V.! pb0, posB)
                  | pa1 == pa0 && odd pa0  -> negate $ horizontalComposition gl (identityCobordism $ layerA pa0, posA) (b V.! pb0, posB)
                  | otherwise              -> zeroCobordism (cols V.! col) (rows V.! row)


data KhovanovComplex c =
    KhovanovComplex
        { legsN     :: !Int
        , dimOffset :: !Int
        , chain     :: !(BoundedChain c)
        }

deriving instance (Cobordism c, Show c, Show (BoundedChain c)) => Show (KhovanovComplex c)

instance RotationAction (KhovanovComplex c) where
    rotationOrder = legsN

    rotateByUnchecked = error "rotate is not implemented"

instance (CannedCobordism c, PreadditiveCobordism c) => PlanarAlgebra (KhovanovComplex c) where
    planarDegree = legsN

    planarPropagator n =
        KhovanovComplex
            { legsN     = 2 * n
            , dimOffset = 0
            , chain     = Singl $ planarPropagator n
            }

    horizontalCompositionUnchecked !gl (!a, !posA) (!b, !posB) =
        KhovanovComplex
            { legsN     = legsN a + legsN b - 2 * gl
            , dimOffset = dimOffset a + dimOffset b
            , chain     = glueChains gl (chain a, posA) (chain b, posB)
            }


complexDim :: KhovanovComplex c -> Int
complexDim = chainSpan . chain


testComplexBorders :: (PreadditiveCobordism c) => KhovanovComplex c -> Bool
testComplexBorders (KhovanovComplex { chain = Singl _ }) = True
testComplexBorders (KhovanovComplex { chain = Chain b }) = V.all isZeroCobordism (V.zipWith (∘) (V.tail b) b)


overCrossingComplex, underCrossingComplex :: KhovanovComplex (DottedCobordism Integer)
overCrossingComplex =
    KhovanovComplex
        { legsN     = 4
        , dimOffset = -1
        , chain     = Chain $ V.singleton $ M.singleton saddleCobordism
        }
underCrossingComplex =
    KhovanovComplex
        { legsN     = 4
        , dimOffset = -1
        , chain     = Chain $ V.singleton $ M.singleton saddleCobordism'
        }


khovanovComplex :: TangleDiagram -> KhovanovComplex (DottedCobordism Integer)
khovanovComplex =
    reduceWithDefaultStrategy
        (\ v -> let d = nthOutcomingDart v 0
                in if passOver d
                    then overCrossingComplex
                    else underCrossingComplex
        )
