module Math.Topology.KnotTh.Invariants.KhovanovHomology
    ( module Math.Topology.KnotTh.Cobordism.DottedCobordism
    , KhovanovComplex(..)
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


data KhovanovComplex c =
    KhovanovComplex
        { legsN     :: !Int
        , dimOffset :: !Int
        , borders   :: !(V.Vector (M.CobordismMatrix c))
        }

instance RotationAction (KhovanovComplex c) where
    rotationOrder = legsN

    rotateByUnchecked = error "rotate is not implemented"

instance (CannedCobordism c, PreadditiveCobordism c) => PlanarAlgebra (KhovanovComplex c) where
    planarDegree = legsN

    planarPropagator n =
        KhovanovComplex
            { legsN     = 2 * n
            , dimOffset = -1
            , borders   = V.empty
            }

    horizontalCompositionUnchecked !gl (!a, !posA) (!b, !posB) =
        KhovanovComplex
            { legsN     = legsN a + legsN b - 2 * gl
            , dimOffset = dimOffset a + dimOffset b
            , borders   =
                let dimA = V.length $ borders a
                    dimB = V.length $ borders b

                    width d = max 0 $ (min d dimA) - (max 0 $ d - dimB) + 1

                    coords d w =
                        let pa = w + max 0 (d - dimB)
                        in (pa, d - pa)

                in V.generate (dimA + dimB - 1) $ \ d ->
                    let cols = V.generate (width d) $ \ w ->
                                let (pa, pb) = coords d w
                                in horizontalComposition gl (cobordismBorder0 $ borders a V.! pa, posA)
                                                            (cobordismBorder0 $ borders b V.! pb, posB)

                        rows = V.generate (width $ d + 1) $ \ w ->
                                let (pa, pb) = coords (d + 1) w
                                in if pa > 0
                                    then horizontalComposition gl (cobordismBorder1 $ borders a V.! (pa - 1), posA)
                                                                  (cobordismBorder1 $ borders b V.! pb, posB)
                                    else horizontalComposition gl (cobordismBorder1 $ borders a V.! pa, posA)
                                                                  (cobordismBorder1 $ borders b V.! (pb - 1), posB)

                    in M.flatten $ M.generate cols rows $ \ row col ->
                        let (pa0, pb0) = coords d col
                            arrowA = borders a V.! pa0
                            arrowB = borders b V.! pb0
                            (pa1, pb1) = coords (d + 1) row
                        in (if odd pa0 then negate else id) $
                            if | pa1 == pa0 + 1 && pb1 == pb0 -> horizontalComposition gl (arrowA, posA) (identityCobordism $ cobordismBorder0 arrowB, posB)
                               | pa1 == pa0 && pb1 == pb0 + 1 -> horizontalComposition gl (identityCobordism $ cobordismBorder0 arrowA, posA) (arrowB, posB)
                               | otherwise                    -> zeroCobordism (rows V.! row) (cols V.! col)
            }


testComplexBorders :: (PreadditiveCobordism c) => KhovanovComplex c -> Bool
testComplexBorders comp =
    V.all isZeroCobordism $ V.zipWith (∘) (V.tail $ borders comp) (borders comp)


overCrossingComplex, underCrossingComplex :: KhovanovComplex (DottedCobordism Integer)
overCrossingComplex =
    KhovanovComplex
        { legsN     = 4
        , dimOffset = -1
        , borders   = V.singleton (M.singleton saddleCobordism)
        }
underCrossingComplex =
    KhovanovComplex
        { legsN     = 4
        , dimOffset = -1
        , borders   = V.singleton (M.singleton saddleCobordism')
        }


khovanovComplex :: TangleDiagram -> KhovanovComplex (DottedCobordism Integer)
khovanovComplex =
    reduceWithDefaultStrategy
        (\ v -> let d = nthOutcomingDart v 0
                in if passOver d
                    then overCrossingComplex
                    else underCrossingComplex
        )
