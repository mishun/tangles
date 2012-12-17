{-# LANGUAGE TemplateHaskell #-}
module Math.KnotTh.SurfaceLink
	( module Math.KnotTh.Knotted
	, SurfaceLink
	, Crossing
	, Face
	, Dart
	, crossingSurfaceLink
	, faceSurfaceLink
	, dartSurfaceLink
	, fromList
	, fromListST
	) where

import Math.KnotTh.Knotted
import Math.KnotTh.Knotted.TH.Link


produceKnottedInstance [d|
	data SurfaceLink ct = SurfaceLink
		{ --faceCount :: {-# UNPACK #-} !Int
		}
	|]


data Face ct = Face !(SurfaceLink ct) {-# UNPACK #-} !Int


{-# INLINE faceSurfaceLink #-}
faceSurfaceLink :: Face ct -> SurfaceLink ct
faceSurfaceLink (Face l _) = l


instance SurfaceKnotted SurfaceLink Crossing Face Dart where

	numberOfFaces _ = 0 --faceCount

	nthFace link i
		| i < 1 || i > numberOfFaces link  = error "nthFace: out of bound"
		| otherwise                        = Face link (i - 1)

	faceOwner = faceSurfaceLink

	faceIndex (Face _ i) = i + 1
