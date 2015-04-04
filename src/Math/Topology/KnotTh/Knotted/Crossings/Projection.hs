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


instance Crossing ProjectionCrossing where
    mirrorCrossing = id
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
