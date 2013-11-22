{-# LANGUAGE RankNTypes #-}
module Math.Topology.KnotTh.Invariants.KnotPolynomials
    ( module X
    , SkeinRelation(..)
    , reduceSkeinWithStrategy
    , reduceSkeinStd
    , standardReductionStrategy
    , skeinRelationPostMinimization
    , skeinRelationPreMinimization
    ) where

import Math.Algebra.PlanarAlgebra.Reduction as X
import Math.Topology.KnotTh.Tangle as X


class (Functor f, PlanarStateSum (f p)) => SkeinRelation f p where
    skeinLPlus, skeinLMinus :: f p
    finalNormalization      :: NATangle -> f p -> f p
    invertCrossingsAction   :: f p -> f p
    takeAsScalar            :: f p -> p

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


skeinRelationPostMinimization :: (Ord (f p), SkeinRelation f p) => (NATangle -> f p) -> NATangle -> f p 
skeinRelationPostMinimization invariant tangle = minimum $ do
    let l = numberOfLegs tangle
        p = invariant tangle
    rotation <- if l == 0 then [id] else map rotateState [0 .. l - 1]
    reflection <- [id, mirrorState]
    inv <- [id, invertCrossingsAction]
    return $ inv $ reflection $ rotation p


skeinRelationPreMinimization :: (Ord (f p), SkeinRelation f p) => (NATangle -> f p) -> NATangle -> f p 
skeinRelationPreMinimization invariant tangle = minimum $ do
    let l = numberOfLegs tangle
    rotation <- if l == 0 then [id] else map rotateTangle [0 .. l - 1]
    reflection <- [id, mirrorTangle]
    inv <- [id, invertCrossings]
    return $ invariant $ inv $ rotation $ reflection tangle
