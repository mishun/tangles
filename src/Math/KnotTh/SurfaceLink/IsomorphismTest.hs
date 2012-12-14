{-# LANGUAGE UnboxedTuples #-}
module Math.KnotTh.SurfaceLink.IsomorphismTest
	( isomorphismTest
	) where

import Data.Array.Unboxed (UArray)
import Math.Algebra.RotationDirection (RotationDirection, ccw, cw)
import Math.KnotTh.SurfaceLink


isomorphismTest :: (CrossingType ct) => SurfaceLink ct -> UArray Int Int
isomorphismTest link
	| numberOfCrossings link > 127  = error "isomorphismTest: too many crossings"
	| otherwise                     = minimum [ codeWithDirection dir dart | dart <- allDarts link, dir <- [ccw, cw] ]


codeWithDirection :: (CrossingType ct) => RotationDirection -> Dart ct -> UArray Int Int
codeWithDirection !dir !dart = undefined
