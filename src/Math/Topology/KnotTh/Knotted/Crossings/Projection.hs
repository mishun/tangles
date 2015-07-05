{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Knotted.Crossings.Projection
    ( ProjectionCrossing
    , projectionCrossing
    , projectionCrossings
    , projection
    ) where

import Control.DeepSeq
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.Threads


data ProjectionCrossing = ProjectionCrossing deriving (Eq)

instance Show ProjectionCrossing where
    show _ = "projectionCrossing"

instance Read ProjectionCrossing where
    readsPrec _ s = do
        ("projectionCrossing", t) <- lex s
        return (projectionCrossing, t)

instance NFData ProjectionCrossing

instance RotationAction ProjectionCrossing where
    rotationOrder _ = 4

    {-# INLINE rotateBy #-}
    rotateBy _ = id

instance DihedralAction ProjectionCrossing where
    {-# INLINE mirrorIt #-}
    mirrorIt = id

instance Crossing ProjectionCrossing where
    globalTransformations _ = Nothing
    crossingCode _ _ = (# 0, 0 #)
    crossingCodeWithGlobal _ _ _ = (# 0, 0 #)

instance ThreadedCrossing ProjectionCrossing


projectionCrossing :: ProjectionCrossing
projectionCrossing = ProjectionCrossing


projectionCrossings :: [ProjectionCrossing]
projectionCrossings = [projectionCrossing]


projection :: (Knotted k) => k a -> k ProjectionCrossing
projection = fmap (const projectionCrossing)
