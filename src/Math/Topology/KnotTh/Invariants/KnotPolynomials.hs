{-# LANGUAGE RankNTypes #-}
module Math.Topology.KnotTh.Invariants.KnotPolynomials
    ( module Math.Algebra.PlanarAlgebra.Reduction
    , SkeinRelation(..)
    , reduceSkeinWithStrategy
    , reduceSkeinStd
    , standardReductionStrategy
    , skeinRelationPostMinimization
    , skeinRelationMidMinimization
    , skeinRelationPreMinimization
    ) where

import Math.Algebra.PlanarAlgebra.Reduction
import Math.Topology.KnotTh.Tangle


class (Functor f, PlanarStateSum (f p)) => SkeinRelation f p where
    skeinLPlus, skeinLMinus :: f p
    finalNormalization      :: (KnottedPlanar k) => k DiagramCrossing -> f p -> f p
    invertCrossingsAction   :: f p -> f p
    takeAsScalar            :: f p -> p

    finalNormalization _ = id


reduceSkeinWithStrategy :: (SkeinRelation f p) => TangleDiagram -> Strategy (f p) -> f p
reduceSkeinWithStrategy tangle =
    reduceWithStrategy tangle
        (\ v -> let d = nthOutcomingDart v 0
                in if passOver d
                    then skeinLPlus
                    else skeinLMinus
        )


reduceSkeinStd :: (SkeinRelation f p) => TangleDiagram -> f p
reduceSkeinStd tangle =
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


skeinRelationPostMinimization :: (Ord (f p), SkeinRelation f p) => (TangleDiagram -> f p) -> TangleDiagram -> f p 
skeinRelationPostMinimization invariant tangle = minimum $ do
    let l = numberOfLegs tangle
        p = invariant tangle
    rot <- if l == 0 then [id]
                     else map rotateState [0 .. l - 1]
    refl <- [id, mirrorState]
    inv <- [id, invertCrossingsAction]
    return $ inv $ refl $ rot p


skeinRelationMidMinimization :: (Ord (f p), SkeinRelation f p) => (TangleDiagram -> f p) -> TangleDiagram -> f p
skeinRelationMidMinimization invariant tangle = minimum $ do
    let l = numberOfLegs tangle
    tangle' <- [tangle, invertCrossings tangle]
    let p = invariant tangle'
    rot <- if l == 0 then [id]
                     else map rotateState [0 .. l - 1]
    refl <- [id, mirrorState]
    return $ refl $ rot p


skeinRelationPreMinimization :: (Ord (f p), SkeinRelation f p) => (TangleDiagram -> f p) -> TangleDiagram -> f p 
skeinRelationPreMinimization invariant tangle = minimum $ do
    inv <- [id, invertCrossings]
    tangle' <- allOrientationsOf tangle
    return $ invariant $ inv tangle'
