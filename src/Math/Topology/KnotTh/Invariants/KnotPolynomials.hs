{-# LANGUAGE RankNTypes #-}
module Math.Topology.KnotTh.Invariants.KnotPolynomials
    ( module X
    , SkeinRelation(..)
    , reduceSkeinWithStrategy
    , reduceSkeinStd
    , standardReductionStrategy
    ) where

import Math.Algebra.PlanarAlgebra.Reduction as X
import Math.Topology.KnotTh.Tangle as X


class (Functor f, PlanarStateSum (f p)) => SkeinRelation f p where
    skeinLPlus, skeinLMinus :: f p
    finalNormalization      :: NATangle -> f p -> f p
    invertCrossingsAction   :: f p -> f p

    finalNormalization _ = id


reduceSkeinWithStrategy :: (SkeinRelation f p) => NATangle -> Strategy (f p) -> f p
reduceSkeinWithStrategy tangle =
    reduceWithStrategy tangle
        (\ v -> let d = nthOutcomingDart v 0
                in if passOver d
                    then skeinLPlus
                    else skeinLMinus
        )


reduceSkeinStd :: (SkeinRelation f p) => NATangle -> f p
reduceSkeinStd tangle =
    finalNormalization tangle $
        reduceSkeinWithStrategy tangle standardReductionStrategy


standardReductionStrategy :: Strategy a
standardReductionStrategy =
    let try [] = error "standardReductionStrategy: no reduction places left"
        try ((v, i) : t) = do
            (u, _) <- oppositeM (v, i)
            if u /= v
                then return $! Contract (v, i)
                else try t
    in try
