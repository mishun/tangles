{-# LANGUAGE TypeFamilies #-}
module Math.Algebra.PlanarAlgebra.Definition
    ( PlanarState(..)
    , PlanarDiagram(..)
    , PlanarAlgebra(..)
    , SurfaceDiagram(..)
    ) where

import Data.Array.Unboxed (UArray)


class PlanarState s where
    stateDegree   :: s -> Int
    rotateState   :: Int -> s -> s
    mirrorState   :: s -> s
    loopState     :: Int -> (s, Int) -> (s, UArray Int Int)
    connectStates :: Int -> (s, Int) -> (s, Int) -> (s, UArray Int Int, UArray Int Int) 


class PlanarDiagram a where
    numberOfVertices :: a t -> Int
    nthVertex        :: a t -> Int -> Vertex a t

    data Vertex a t
    vertexDegree     :: Vertex a t -> Int
    vertexOwner      :: Vertex a t -> a t
    vertexPlace      :: Vertex a t -> Int
    nthOutcomingDart :: Vertex a t -> Int -> Dart a t

    data Dart a t
    dartOwner        :: Dart a t -> a t
    incidentVertex   :: Dart a t -> Vertex a t
    dartPlace        :: Dart a t -> Int
    opposite         :: Dart a t -> Dart a t
    nextCW, nextCCW  :: Dart a t -> Dart a t
    isDart           :: Dart a t -> Bool


class (PlanarDiagram a) => PlanarAlgebra a where
    borderDegree :: a t -> Int
    nthLeg       :: a t -> Int -> Dart a t
    isLeg        :: Dart a t -> Bool
    legPlace     :: Dart a t -> Int


class (PlanarDiagram a) => SurfaceDiagram a where
    numberOfFaces :: a t -> Int
    nthFace       :: a t -> Int -> Face a t

    data Face a t
    faceDegree    :: Face a t -> Int
    faceOwner     :: Face a t -> a t
    facePlace     :: Face a t -> Int
