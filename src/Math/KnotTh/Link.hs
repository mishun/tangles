{-# LANGUAGE TemplateHaskell #-}
module Math.KnotTh.Link
	( module Math.KnotTh.Knotted
	, Link
	, Crossing
	, Dart
	, crossingLink
	, dartLink
	, implode
	, explode
	, allThreads
	) where

import Data.List (foldl')
import qualified Data.Set as Set
import Math.KnotTh.Knotted
import Math.KnotTh.Knotted.TH.Knotted


produceKnottedInstance
	[d| data Link ct = Link {} |]
	defaultKnottedInstance


allThreads :: Link ct -> [[(Dart ct, Dart ct)]]
allThreads =
	let extractThread (threads, vis) start
		| Set.member start vis  = (threads, vis)
		| otherwise             =
			let thread =
				let walk list d
					| d == start  = list
					| otherwise   = walk ((opposite d, d) : list) $ continuation $ opposite d
				in walk [(opposite start, start)] $ continuation $ opposite start
			in (thread : threads, foldl' (\ s (a, b) -> Set.insert b $ Set.insert a s) vis thread)
	in fst . foldl extractThread ([], Set.empty) . allDarts
