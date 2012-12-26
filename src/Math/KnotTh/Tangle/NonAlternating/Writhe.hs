module Math.KnotTh.Tangle.NonAlternating.Writhe
	( selfWrithe
	, selfWritheArray
	) where

import Data.Array.Unboxed (UArray, elems)
import Data.Array.ST (STArray, STUArray, runSTUArray, newArray, newArray_, readArray, writeArray)
import Control.Monad.ST (ST)
import Control.Monad (forM_, when)
import Math.KnotTh.Tangle.NonAlternating


selfWrithe :: NonAlternatingTangle -> Int
selfWrithe = sum . elems . selfWritheArray


selfWritheArray :: NonAlternatingTangle -> UArray Int Int
selfWritheArray tangle = runSTUArray $ do
	let n = numberOfCrossings tangle
	w <- newArray (1, n) 0
	vis <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
	inp <- newArray_ (1, n) :: ST s (STArray s Int (Dart ArbitraryCrossing))

	forM_ (zip [1 ..] $ allThreads tangle) $ \ (!i, thread) ->
		forM_ thread $ \ (!d, _) -> when (isDart d) $ do
			let c = crossingIndex $ incidentCrossing d
			v <- readArray vis c
			if v /= 0
				then when (v == i) $ readArray inp c >>= writeArray w c . flip writhe d
				else writeArray vis c i >> writeArray inp c d

	return $! w
