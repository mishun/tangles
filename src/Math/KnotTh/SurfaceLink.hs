module Math.KnotTh.SurfaceLink
	( module Math.KnotTh.SurfaceKnotted
	, SurfaceLink
	, Dart
	) where

import Math.KnotTh.SurfaceKnotted


data Dart ct

data Crossing ct

data Face ct

data SurfaceLink ct


instance Knotted SurfaceLink Crossing Dart where


instance SurfaceKnotted SurfaceLink Crossing Face Dart where
