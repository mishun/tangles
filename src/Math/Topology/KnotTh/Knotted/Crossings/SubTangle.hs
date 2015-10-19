{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Knotted.Crossings.SubTangle
    ( DirectSumDecompositionType(..)
    , SubTangleCrossing
    , SubTangleTangle
    , extractSubTangle
    , substituteSubTangles
    , makeSubTangleCrossing
    , makeSubTangleCrossing'
    , isLonerInSub
    , isLonerInVertex
    , isPassingLoner
    , numberOfVerticesInSub
    , numberOfVerticesInVertex
    , directSumDecompositionType
    , directSumDecompositionTypeInVertex
    , possibleSubTangleOrientations
    ) where

import Control.DeepSeq
import Text.Printf
import Math.Topology.KnotTh.Algebra.Dihedral.D4
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Tangle


data DirectSumDecompositionType = NonDirectSumDecomposable
                                | DirectSum01x23
                                | DirectSum12x30
    deriving (Eq, Show)


changeSumType :: DirectSumDecompositionType -> DirectSumDecompositionType
changeSumType NonDirectSumDecomposable = NonDirectSumDecomposable
changeSumType DirectSum01x23 = DirectSum12x30
changeSumType DirectSum12x30 = DirectSum01x23


data SubTangleCrossing a =
    SubTangleCrossing
        { code         :: {-# UNPACK #-} !Int
        , orientation  :: {-# UNPACK #-} !D4
        , symmetry     :: !(SubGroup D4)
        , sumType      :: !DirectSumDecompositionType
        , subTangle    :: Tangle4 a
        }


type SubTangleTangle a = Tangle (SubTangleCrossing a)

instance (NFData a) => NFData (SubTangleCrossing a) where
    rnf s = rnf (subTangle s) `seq` s `seq` ()

instance (Show a) => Show (SubTangleCrossing a) where
    show s =
        printf "(%s / %s | %s - %s)"
            (show $ orientation s)
            (show $ symmetry s)
            (show $ subTangle s)
            (show $ sumType s)

instance RotationAction (SubTangleCrossing a) where
    rotationOrder _ = 4

    rotateByUnchecked !rot s = s { orientation = fromRotation rot ∘ orientation s }

instance MirrorAction (SubTangleCrossing a) where
    mirrorIt s = s { orientation = d4E ∘ orientation s }

instance GroupAction D4 (SubTangleCrossing a) where
    transform g s = s { orientation = g ∘ orientation s }

instance Crossing (SubTangleCrossing a) where
    -- TODO: actually this is not right
    flipCrossing = id

    {-# INLINE globalTransformations #-}
    globalTransformations _ = Nothing

    {-# INLINE crossingCode #-}
    crossingCode dir d =
        let p = beginPlace d
            cr = vertexContent $ beginVertex d
            t = fromReflectionRotation (isClockwise dir) (-p) ∘ orientation cr
        in (# code cr, equivalenceClassId (symmetry cr) t #)

    {-# INLINE crossingCodeWithGlobal #-}
    crossingCodeWithGlobal global dir d =
        let p = beginPlace d
            cr = vertexContent $ beginVertex d
            t = fromReflectionRotation (isClockwise dir) (-p) ∘ (orientation cr ∘ global)
        in (# code cr, equivalenceClassId (symmetry cr) t #)


extractSubTangle :: (Crossing a) => SubTangleCrossing a -> Tangle4 a
extractSubTangle c = transform (orientation c) (subTangle c)


substituteSubTangles :: (Crossing a, Surgery k) => k (SubTangleCrossing a) -> k a
substituteSubTangles = multiSurgery . fmap extractSubTangle


makeSubTangleCrossing :: Tangle4 a -> SubGroup D4 -> DirectSumDecompositionType -> Int -> SubTangleCrossing a
makeSubTangleCrossing tangle newSymmetry newSumType newCode =
    SubTangleCrossing
        { code        = newCode
        , orientation = d4I
        , symmetry    = newSymmetry
        , sumType     = newSumType
        , subTangle   = tangle
        }


makeSubTangleCrossing' :: (Crossing a) => Tangle4 (SubTangleCrossing a) -> SubGroup D4 -> DirectSumDecompositionType -> Int -> SubTangleCrossing a
makeSubTangleCrossing' = makeSubTangleCrossing . substituteSubTangles


isLonerInSub :: SubTangleCrossing a -> Bool
isLonerInSub = (== 1) . numberOfVertices . subTangle


isLonerInVertex :: (VertexDiagram d) => Vertex d (SubTangleCrossing a) -> Bool
isLonerInVertex = isLonerInSub . vertexContent


isPassingLoner :: (VertexDiagram d) => Dart d (SubTangleCrossing a) -> Bool
isPassingLoner = isLonerInVertex . beginVertex


numberOfVerticesInSub :: SubTangleCrossing a -> Int
numberOfVerticesInSub = numberOfVertices . subTangle


numberOfVerticesInVertex :: (VertexDiagram d) => Vertex d (SubTangleCrossing a) -> Int
numberOfVerticesInVertex = numberOfVerticesInSub . vertexContent


directSumDecompositionType :: SubTangleCrossing a -> DirectSumDecompositionType
directSumDecompositionType c | f          = changeSumType st
                             | otherwise  = st
    where st = sumType c
          f = reflection (orientation c) /= odd (crossingLegIdByDartId c 0)


directSumDecompositionTypeInVertex :: (Knotted k) => Dart k (SubTangleCrossing a) -> DirectSumDecompositionType
directSumDecompositionTypeInVertex d | f          = changeSumType st
                                     | otherwise  = st
    where c = vertexContent $ beginVertex d
          st = sumType c
          f = reflection (orientation c) /= odd (crossingLegIdByDart d)


possibleSubTangleOrientations :: SubTangleCrossing a -> Maybe D4 -> [SubTangleCrossing a]
possibleSubTangleOrientations base extra =
    let s = symmetry base
        orient = equvalenceClassRepresentatives s
    in map (\ g -> base { orientation = g }) $
        case extra of
            Nothing -> orient
            Just h  -> filter (\ g -> equivalenceClassId s g <= equivalenceClassId s (h ∘ g)) orient


{-# INLINE crossingLegIdByDartId #-}
crossingLegIdByDartId :: SubTangleCrossing a -> Int -> Int
crossingLegIdByDartId cr = permutePoint (inverse $ orientation cr)


{-# INLINE crossingLegIdByDart #-}
crossingLegIdByDart :: (Knotted k) => Dart k (SubTangleCrossing a) -> Int
crossingLegIdByDart d = crossingLegIdByDartId (vertexContent $ beginVertex d) (beginPlace d)
