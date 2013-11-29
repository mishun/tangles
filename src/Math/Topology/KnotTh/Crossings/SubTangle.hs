{-# LANGUAGE UnboxedTuples #-}
module Math.Topology.KnotTh.Crossings.SubTangle
    ( makeCrossingCache'
    , possibleSubTangleOrientations
    , DirectSumDecompositionType(..)
    , SubTangleCrossingType
    , subTangle
    , SubTangleCrossing
    , SubTangleTangle
    , crossingFromTangle
    , crossingFromTangle'
    , isLonerInVertex
    , numberOfVerticesAfterSubstitution
    , directSumDecompositionTypeInVertex
    , directSumDecompositionTypeOfCrossing
    , substituteTangles
    ) where

import Data.Array.Unboxed (UArray, (!), listArray)
import Control.DeepSeq
import Control.Monad (guard)
import Text.Printf
import qualified Math.Algebra.RotationDirection as R
import qualified Math.Algebra.Group.Dn as Dn
import qualified Math.Algebra.Group.D4 as D4
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Tangle


class (Eq t) => CrossingType t where
    crossingTypeCode :: t -> Int
    localCrossingSymmetry :: t -> D4.D4SubGroup


data CrossingCache t =
    CrossingCache
        { code         :: {-# UNPACK #-} !Int
        , orientation  :: {-# UNPACK #-} !D4.D4
        , symmetry     :: !D4.D4SubGroup
        , crossingType :: !t
        }


instance (Eq t) => Eq (CrossingCache t) where
    (==) a b = symmetry a == symmetry b
        && D4.equivalenceClassId (symmetry a) (orientation a) == D4.equivalenceClassId (symmetry b) (orientation b)
        && crossingType a == crossingType b


instance (NFData t) => NFData (CrossingCache t) where
    rnf cr = rnf (crossingType cr) `seq` cr `seq` ()


instance (Show t) => Show (CrossingCache t) where
    show c = printf "(%s / %s | %s)"
            (show $ orientation c)
            (show $ symmetry c)
            (show $ crossingType c)


instance (CrossingType t, Read t) => Read (CrossingCache t) where
    readsPrec _ =
        readParen True $ \ s0 -> do
            (g, s1) <- reads s0
            ("/", s2) <- lex s1
            (sym, s3) <- reads s2
            ("|", s4) <- lex s3
            (ct, s5) <- reads s4
            let cs = makeCrossingCache ct g
            guard $ symmetry cs == sym
            return (cs, s5)


{-# INLINE isCrossingOrientationInverted #-}
isCrossingOrientationInverted :: CrossingCache t -> Bool
isCrossingOrientationInverted = D4.hasReflection . orientation


{-# INLINE crossingLegIdByDartId #-}
crossingLegIdByDartId :: CrossingCache t -> Int -> Int
crossingLegIdByDartId cr = D4.permute (D4.inverse $ orientation cr)


{-# INLINE dartIdByCrossingLegId #-}
dartIdByCrossingLegId :: CrossingCache t -> Int -> Int
dartIdByCrossingLegId cr = D4.permute (orientation cr)


mapOrientation :: (D4.D4 -> D4.D4) -> CrossingCache t -> CrossingCache t
mapOrientation f crossing = crossing { orientation = f $ orientation crossing }


makeCrossingCache :: (CrossingType t) => t -> D4.D4 -> CrossingCache t
makeCrossingCache ct g =
    CrossingCache
        { code         = crossingTypeCode ct
        , orientation  = g
        , symmetry     = localCrossingSymmetry ct
        , crossingType = ct
        }


makeCrossingCache' :: (CrossingType t) => t -> CrossingCache t
makeCrossingCache' = flip makeCrossingCache D4.i


{-# INLINE vertexCrossingType #-}
vertexCrossingType :: (Knotted k) => Vertex k (CrossingCache t) -> t
vertexCrossingType = crossingType . vertexCrossing


{-# INLINE isVertexCrossingOrientationInverted #-}
isVertexCrossingOrientationInverted :: (Knotted k) => Vertex k (CrossingCache t) -> Bool
isVertexCrossingOrientationInverted = isCrossingOrientationInverted . vertexCrossing


{-# INLINE crossingLegIdByDart #-}
crossingLegIdByDart :: (Knotted k) => Dart k (CrossingCache t) -> Int
crossingLegIdByDart d = crossingLegIdByDartId (vertexCrossing $ beginVertex d) (beginPlace d)


{-# INLINE dartByCrossingLegId #-}
dartByCrossingLegId :: (Knotted k) => Vertex k (CrossingCache t) -> Int -> Dart k (CrossingCache t)
dartByCrossingLegId c = nthOutcomingDart c . dartIdByCrossingLegId (vertexCrossing c)


instance (CrossingType t) => Crossing (CrossingCache t) where
    {-# INLINE mirrorCrossing #-}
    mirrorCrossing = mapOrientation (D4.ec D4.<*>)

    {-# INLINE globalTransformations #-}
    globalTransformations _ = Nothing

    {-# INLINE crossingCode #-}
    crossingCode dir d =
        let p = beginPlace d
            cr = vertexCrossing $ beginVertex d
            t = D4.fromReflectionRotation (R.isClockwise dir) (-p) D4.<*> orientation cr
        in (# code cr, D4.equivalenceClassId (symmetry cr) t #)

    {-# INLINE crossingCodeWithGlobal #-}
    crossingCodeWithGlobal global dir d =
        let p = beginPlace d
            cr = vertexCrossing $ beginVertex d
            t = D4.fromReflectionRotation (R.isClockwise dir) (-p) D4.<*> (orientation cr D4.<*> global)
        in (# code cr, D4.equivalenceClassId (symmetry cr) t #)


possibleSubTangleOrientations :: SubTangleCrossingType a -> Maybe D4.D4 -> [SubTangleCrossing a]
possibleSubTangleOrientations ct extra =
    let s = localCrossingSymmetry ct
        orient = D4.equvalenceClassRepresentatives s
    in map (makeCrossingCache ct) $
        case extra of
            Nothing -> orient
            Just h  -> filter (\ g -> D4.equivalenceClassId s g <= D4.equivalenceClassId s (h D4.<*> g)) orient


data DirectSumDecompositionType = NonDirectSumDecomposable
                                | DirectSum01x23
                                | DirectSum12x30
    deriving (Eq, Show)


changeSumType :: DirectSumDecompositionType -> DirectSumDecompositionType
changeSumType NonDirectSumDecomposable = NonDirectSumDecomposable
changeSumType DirectSum01x23 = DirectSum12x30
changeSumType DirectSum12x30 = DirectSum01x23


data SubTangleCrossingType a =
    SubTangle
        { _code     :: {-# UNPACK #-} !Int
        , _symmetry :: !D4.D4SubGroup
        , _sumType  :: !DirectSumDecompositionType
        , subTangle :: Tangle a
        }


instance Eq (SubTangleCrossingType a) where
    (==) a b = _code a == _code b


instance (NFData a) => NFData (SubTangleCrossingType a) where
    rnf x = rnf (subTangle x) `seq` x `seq` ()


instance CrossingType (SubTangleCrossingType a) where
    crossingTypeCode = _code
    localCrossingSymmetry = _symmetry


instance (Show a) => Show (SubTangleCrossingType a) where
    show cr =
        printf "(SubTangle %i %s %s (%s))"
            (_code cr)
            (show $ _symmetry cr)
            (show $ _sumType cr)
            (show $ subTangle cr)


type SubTangleCrossing a = CrossingCache (SubTangleCrossingType a)

type SubTangleTangle a = Tangle (SubTangleCrossing a)


crossingFromTangle :: Tangle a -> Dn.DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossingType a
crossingFromTangle tangle newSymmetry sumType newCode
    | l /= 4     = error $ printf "crossingFromTangle: tangle must have 4 legs, %i found" l
    | l' /= 4    = error $ printf "crossingFromTangle: symmetry group must have 4 points, %i found" l'
    | lp > 0     = error $ printf "crossingFromTangle: tangle contains %i free loops" lp
    | otherwise  =
        SubTangle
            { _code     = newCode
            , _symmetry = D4.fromDnSubGroup newSymmetry
            , _sumType  = sumType
            , subTangle = tangle
            }
    where
        l = numberOfLegs tangle
        l' = Dn.pointsUnderSubGroup newSymmetry
        lp = numberOfFreeLoops tangle


crossingFromTangle' :: (Crossing a) => SubTangleTangle a -> Dn.DnSubGroup -> DirectSumDecompositionType -> Int -> SubTangleCrossingType a
crossingFromTangle' tangle = crossingFromTangle (substituteTangles tangle)


{-# INLINE isLonerInVertex #-}
isLonerInVertex :: (Knotted k) => Vertex k (SubTangleCrossing a) -> Bool
isLonerInVertex = (== 1) . numberOfVerticesInVertex


numberOfVerticesAfterSubstitution :: (Knotted k) => k (SubTangleCrossing a) -> Int
numberOfVerticesAfterSubstitution = sum . map numberOfVerticesInVertex . allVertices


directSumDecompositionTypeInVertex :: (Knotted k) => Dart k (SubTangleCrossing a) -> DirectSumDecompositionType
directSumDecompositionTypeInVertex d
    | f          = changeSumType st
    | otherwise  = st
    where
        st = _sumType $ vertexCrossingType $ beginVertex d
        f = isVertexCrossingOrientationInverted (beginVertex d) /= odd (crossingLegIdByDart d)


directSumDecompositionTypeOfCrossing :: SubTangleCrossing a -> DirectSumDecompositionType
directSumDecompositionTypeOfCrossing c
    | f          = changeSumType st
    | otherwise  = st
    where
        st = _sumType $ crossingType c
        f = isCrossingOrientationInverted c /= odd (crossingLegIdByDartId c 0)


substituteTangles :: (Crossing a) => SubTangleTangle a -> Tangle a
substituteTangles tangle =
{-    tensorSubst 1 (\ v ->
            let c = vertexCrossing v
                g = orientation c
            in transformTangle (Dn.fromReflectionRotation 4 (D4.hasReflection g, D4.rotation g)) $
                subTangle $ crossingType c
        ) tangle-}
    implode
        ( numberOfFreeLoops tangle
        , map oppositeExt $ allLegs tangle
        , do
            b <- allVertices tangle
            let rev = isVertexCrossingOrientationInverted b
            c <- allVertices $ tangleInVertex b
            let nb = map (oppositeInt b) $ outcomingDarts c
            let st | rev        = mirrorCrossing $ vertexCrossing c
                   | otherwise  = vertexCrossing c
            return (if rev then reverse nb else nb, st)
        )
    where
        offset :: UArray Int Int
        offset = listArray (vertexIndicesRange tangle) $
            scanl (\ i c -> i + numberOfVerticesInVertex c) 0 $
                allVertices tangle

        oppositeInt b u | isLeg v                                = oppositeExt $ dartByCrossingLegId b (legPlace v)
                        | isVertexCrossingOrientationInverted b  = (w, 3 - beginPlace v)
                        | otherwise                              = (w, beginPlace v)
            where
                v = opposite u
                c = beginVertex v
                w = (offset ! vertexIndex b) + vertexIndex c

        oppositeExt u | isLeg v    = (0, legPlace v)
                      | otherwise  = oppositeInt c $ subTangleLegFromDart v
            where
                v = opposite u
                c = beginVertex v


{-# INLINE tangleInVertex #-}
tangleInVertex :: (Knotted k) => Vertex k (SubTangleCrossing a) -> Tangle a
tangleInVertex = subTangle . vertexCrossingType


{-# INLINE numberOfVerticesInVertex #-}
numberOfVerticesInVertex :: (Knotted k) => Vertex k (SubTangleCrossing a) -> Int
numberOfVerticesInVertex = numberOfVertices . tangleInVertex


{-# INLINE subTangleLegFromDart #-}
subTangleLegFromDart :: (Knotted k) => Dart k (SubTangleCrossing a) -> Dart Tangle a
subTangleLegFromDart d = nthLeg (tangleInVertex $ beginVertex d) $ crossingLegIdByDart d
