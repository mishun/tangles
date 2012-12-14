{-# LANGUAGE KindSignatures, FunctionalDependencies #-}
module Math.KnotTh.SurfaceKnotted
	( module Math.KnotTh.Knotted
	, SurfaceKnotted(..)
	, eulerChar
	) where

import Math.KnotTh.Knotted


class (Knotted knot cross dart) => SurfaceKnotted knot cross (face :: * -> *) dart | knot -> face, face -> knot where
	numberOfFaces   :: knot ct -> Int
	nthFace         :: knot ct -> Int -> face ct
	faceOwner       :: face ct -> knot ct
	faceIndex       :: face ct -> Int


eulerChar :: (SurfaceKnotted k c f d) => k ct -> Int
eulerChar k = numberOfCrossings k + numberOfFaces k - numberOfEdges k
