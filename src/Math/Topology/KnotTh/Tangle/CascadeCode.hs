{-# LANGUAGE TypeFamilies #-}
module Math.Topology.KnotTh.Tangle.CascadeCode
    ( CascadePattern(..)
    , decodeCascadeCode
    , decodeCascadeCodeFromPairs
    ) where

import Control.Arrow (first)
import Data.Char (isSpace)
import Text.Printf
import Math.Topology.KnotTh.Knotted
import Math.Topology.KnotTh.Knotted.Crossings.Projection
import Math.Topology.KnotTh.Knotted.Crossings.Diagram
import Math.Topology.KnotTh.Tangle.TangleDef


class (Enum (CascadePattern a)) => CascadeCodePattern a where
    data CascadePattern a :: *
    cascadeCodeRoot :: Tangle a
    decodeCrossing  :: CascadePattern a -> (CascadePattern ProjectionCrossing, Int, Int, a)

instance CascadeCodePattern ProjectionCrossing where
    data CascadePattern ProjectionCrossing = W | X | M
        deriving (Eq, Enum, Show, Read)

    cascadeCodeRoot = toTangle lonerProjection

    decodeCrossing W = (W, 1, 0, ProjectionCrossing)
    decodeCrossing X = (X, 1, 0, ProjectionCrossing)
    decodeCrossing M = (M, 0, -1, ProjectionCrossing)

instance CascadeCodePattern DiagramCrossing where
    data CascadePattern DiagramCrossing = WO | WU | XO | XU | MO | MU
        deriving (Eq, Enum)

    cascadeCodeRoot = toTangle lonerOverCrossing

    decodeCrossing WO = (W, 1, 0, UnderCrossing)
    decodeCrossing WU = (W, 1, 0, OverCrossing)
    decodeCrossing XO = (X, 1, 0, OverCrossing)
    decodeCrossing XU = (X, 1, 0, UnderCrossing)
    decodeCrossing MO = (M, 0, -1, OverCrossing)
    decodeCrossing MU = (M, 0, -1, UnderCrossing)

instance Show (CascadePattern DiagramCrossing) where
    show p = case p of
        WO -> "W+"
        WU -> "W-"
        XO -> "X+"
        XU -> "X-"
        MO -> "M+"
        MU -> "M-"

instance Read (CascadePattern DiagramCrossing) where
    readsPrec _ s = case dropWhile isSpace s of
        'W' : '+' : t -> [(WO, t)]
        'W' : '-' : t -> [(WU, t)]
        'X' : '+' : t -> [(XO, t)]
        'X' : '-' : t -> [(XU, t)]
        'M' : '+' : t -> [(MO, t)]
        'M' : '-' : t -> [(MU, t)]
        _             -> []


decodeCascadeCode :: (CascadeCodePattern a) => [(CascadePattern a, Int)] -> Tangle a
decodeCascadeCode =
    foldl (\ prev (pattern, offset) ->
            let (gl, shift, rot, c) = decodeCrossing pattern
            in rotateBy rot $ vertexOwner $
                glueToBorder
                    (case gl of { W -> 3 ; X -> 2 ; M -> 1 })
                    (prev, offset + shift)
                    c
        ) cascadeCodeRoot


decodeCascadeCodeFromPairs :: [(Int, Int)] -> Tangle ProjectionCrossing
decodeCascadeCodeFromPairs =
    let encode (-1) = W
        encode 0    = X
        encode 1    = M
        encode p    = error $ printf "decodeCascadeCodeFromPairs: expected -1, 0 or 1 as pattern, %i received" p
    in decodeCascadeCode . map (first encode)
