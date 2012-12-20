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
	, testPrime
	) where

import Data.Function (fix)
import Data.Array.ST (STUArray, newArray, newArray_, readArray, writeArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_)
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


testPrime :: SurfaceLink ct -> Bool
testPrime link = runST $ do
	let sz = numberOfCrossings link
	g <- newArray ((1, 1), (sz, sz)) 0 :: ST s (STUArray s (Int, Int) Int)

	forM_ (allCrossings link) $ \ u ->
		forM_ (adjacentCrossings u) $ \ v -> do
			let i = (crossingIndex u, crossingIndex v)
			w <- readArray g i
			writeArray g i $! w + 1

	v <- newArray_ (1, sz) :: ST s (STUArray s Int Int)
	forM_ [1 .. sz] $ \ i ->
		writeArray v i i

	a <- newArray_ (1, sz) :: ST s (STUArray s Int Bool)
	w <- newArray_ (1, sz) :: ST s (STUArray s Int Int)
	na <- newArray_ (1, sz) :: ST s (STUArray s Int Int)

	let setA i x = do
		j <- readArray v i
		writeArray a j x

	cut <- flip fix sz $ \ loop n -> do
		setA 1 True
		forM_ [2 .. n] $ \ i -> do
			setA i False
			writeArray na (i - 1) i
			writeArray w i =<< do
				a <- readArray v 1
				b <- readArray v i
				readArray g (a, b)

		return 0

	return True
