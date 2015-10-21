{-# LANGUAGE MultiParamTypeClasses #-}
module Math.Topology.KnotTh.Invariants.KnotPolynomials
    ( SkeinRelation(..)
    , reduceSkein
    , skeinRelationPostMinimization
    , skeinRelationMidMinimization
    , skeinRelationPreMinimization
    ) where

import Math.Topology.KnotTh.Algebra.PlanarAlgebra.Reduction
import Math.Topology.KnotTh.Tangle


class (Functor f, PlanarAlgebra (f p)) => SkeinRelation f p where
    skeinLPlus, skeinLMinus :: f p
    finalNormalization      :: (Knotted k) => k DiagramCrossing -> f p -> f p
    invertCrossingsAction   :: f p -> f p
    takeAsScalar            :: f p -> p

    finalNormalization _ = id


crossingSkein :: (SkeinRelation f p) => DiagramCrossing -> f p
crossingSkein OverCrossing  = skeinLPlus
crossingSkein UnderCrossing = skeinLMinus


reduceSkein :: (SkeinRelation f p) => TangleDiagram -> f p
reduceSkein = reduceWithDefaultStrategy . fmap crossingSkein


skeinRelationPostMinimization :: (Ord (f p), MirrorAction (f p), SkeinRelation f p) => (TangleDiagram -> f p) -> TangleDiagram -> f p
skeinRelationPostMinimization invariant tangle = minimum $ do
    p <- allOrientationsOf $ invariant tangle
    [p, invertCrossingsAction p]


skeinRelationMidMinimization :: (Ord (f p), MirrorAction (f p), SkeinRelation f p) => (TangleDiagram -> f p) -> TangleDiagram -> f p
skeinRelationMidMinimization invariant tangle = minimum $ do
    p <- map invariant [tangle, transposeCrossings tangle]
    allOrientationsOf p


skeinRelationPreMinimization :: (Ord (f p), SkeinRelation f p) => (TangleDiagram -> f p) -> TangleDiagram -> f p 
skeinRelationPreMinimization invariant tangle = minimum $ do
    inv <- [id, transposeCrossings]
    tangle' <- allOrientationsOf tangle
    return $ invariant $ inv tangle'
