{-# LANGUAGE TypeFamilies, UnboxedTuples #-}
module Math.Topology.KnotTh.Link.Definition.Link
    ( Link
    , emptyLink
    , linkToTangle
    , tangleToLink
    , LinkProjection
    , LinkProjectionVertex
    , LinkProjectionDart
    , LinkDiagram
    , LinkDiagramVertex
    , LinkDiagramDart
    ) where

import Data.Function (fix)
import Data.Maybe (fromMaybe)
import Data.Bits ((.&.), shiftL)
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Control.Monad (void, when)
import Control.Arrow ((***))
import Text.Printf
import qualified Math.Algebra.Group.D4 as D4
import qualified Math.Algebra.RotationDirection as R
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Tangle


newtype Link a = L (Tangle a)


instance PlanarDiagram Link where
    numberOfVertices (L t) = numberOfVertices t
    numberOfEdges (L t) = numberOfEdges t
    nthVertex (L t) n = V (nthVertex t n)
    nthDart (L t) n = D (nthDart t n)
    allVertices (L t) = map V (allVertices t)
    allEdges (L t) = map (D *** D) (allEdges t)
    allHalfEdges (L t) = map D (allHalfEdges t)

    newtype Vertex Link a = V (Vertex Tangle a)
    vertexDegree (V v) = vertexDegree v
    vertexOwner (V v) = L (vertexOwner v)
    vertexIndex (V v) = vertexIndex v
    nthOutcomingDart (V v) n = D (nthOutcomingDart v n)
    outcomingDarts (V v) = map D (outcomingDarts v)

    newtype Dart Link a = D (Dart Tangle a)
    dartOwner (D d) = L (dartOwner d)
    dartIndex (D d) = dartIndex d
    opposite (D d) = D (opposite d)
    beginVertex (D d) = V (beginVertex d)
    beginPlace (D d) = beginPlace d
    nextCCW (D d) = D (nextCCW d)
    nextCW (D d) = D (nextCW d)
    nextBy n (D d) = D (nextBy n d)

    vertexIndicesRange (L t) = vertexIndicesRange t
    dartIndicesRange (L t) = dartIndicesRange t


instance Functor Link where
    fmap f (L t) = L (fmap f t)


instance Knotted Link where
    vertexCrossing (V v) = vertexCrossing v

    type ExplodeType Link a = (Int, [([(Int, Int)], a)])
    explode (L t) = let (f, [], l) = explode t in (f, l)
    implode (f, l) = L (implode (f, [], l))

    unrootedHomeomorphismInvariant link
        | numberOfEdges link == 0  = UV.singleton (numberOfFreeLoops link)
        | otherwise                =
            minimum $ do
                dart <- allHalfEdges link
                dir <- R.bothDirections
                globalG <- fromMaybe [D4.i] $ globalTransformations link
                return $! codeWithDirection globalG dir dart

        where
            codeWithDirection !globalG !dir !start = UV.create $ do
                let n = numberOfVertices link

                index <- UMV.replicate (n + 1) 0
                incoming <- UMV.replicate (n + 1) 0
                queue <- MV.new n
                free <- newSTRef 1

                let {-# INLINE look #-}
                    look !d = do
                        let u = beginVertexIndex d
                        ux <- UMV.unsafeRead index u
                        if ux > 0
                            then do
                                up <- UMV.unsafeRead incoming u
                                return $! (ux `shiftL` 2) + (((beginPlace d - up) * R.directionSign dir) .&. 3)
                            else do
                                nf <- readSTRef free
                                writeSTRef free $! nf + 1
                                UMV.unsafeWrite index u nf
                                UMV.unsafeWrite incoming u (beginPlace d)
                                MV.unsafeWrite queue (nf - 1) d
                                return $! nf `shiftL` 2

                rc <- UMV.replicate (6 * n + 1) 0
                UMV.unsafeWrite rc 0 $! numberOfFreeLoops link

                let {-# INLINE lookAndWrite #-}
                    lookAndWrite !d !offset = do
                        look d >>= UMV.unsafeWrite rc offset
                        return $! offset + 1

                void $ look start
                flip fix 0 $ \ bfs !headI -> do
                    tailI <- readSTRef free
                    when (headI < tailI - 1) $ do
                        input <- MV.unsafeRead queue headI
                        void $ foldMAdjacentDartsFrom input dir lookAndWrite (6 * headI + 3)
                        case crossingCodeWithGlobal globalG dir input of
                            (# be, le #) -> do
                                UMV.unsafeWrite rc (6 * headI + 1) be
                                UMV.unsafeWrite rc (6 * headI + 2) le
                        bfs $! headI + 1

                fix $ \ _ -> do
                    tailI <- readSTRef free
                    when (tailI <= n) $
                        fail "codeWithDirection: disconnected diagram (not implemented)"

                return rc

    isConnected (L t) = isConnected t


instance KnottedPlanar Link where
    numberOfFreeLoops (L t) = numberOfFreeLoops t
    changeNumberOfFreeLoops n (L t) = L (changeNumberOfFreeLoops n t)
    emptyKnotted = L emptyKnotted


instance KnottedWithPrimeTest Link where
    isPrime (L t) = isPrime t


instance (Show a) => Show (Link a) where
    show = printf "implode %s" . show . explode


instance (Show a) => Show (Vertex Link a) where
    show v =
        printf "(Crossing %i %s [ %s ])"
            (vertexIndex v)
            (show $ vertexCrossing v)
            (unwords $ map (show . opposite) $ outcomingDarts v)


instance Show (Dart Link a) where
    show d = let (c, p) = beginPair' d
             in printf "(Dart %i %i)" c p


emptyLink :: Link a
emptyLink = emptyKnotted


linkToTangle :: Link a -> Tangle a
linkToTangle (L t) = t


tangleToLink :: Tangle a -> Link a
tangleToLink t | numberOfLegs t == 0  = L t
               | otherwise            = error "tangleToLink: tangle must have 0 legs"


type LinkProjection = Link ProjectionCrossing
type LinkProjectionVertex = Vertex Link ProjectionCrossing
type LinkProjectionDart = Dart Link ProjectionCrossing


type LinkDiagram = Link DiagramCrossing
type LinkDiagramVertex = Vertex Link DiagramCrossing
type LinkDiagramDart = Dart Link DiagramCrossing
