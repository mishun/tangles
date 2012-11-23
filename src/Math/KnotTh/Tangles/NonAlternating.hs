module Math.KnotTh.Tangles.NonAlternating
	( module Math.KnotTh.Crossings.Arbitrary
	, module Math.KnotTh.Tangles
	, NonAlternatingTangle
	, isAlternating
	, invertCrossings
--	, selfWrithe
--	, linkingNumber
	) where

import qualified Data.Map as Map
import Data.List (foldl')
import Math.Algebra.Group.D4 ((<*>), c)
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Tangles
import Math.KnotTh.Tangles.Paths


type NonAlternatingTangle = Tangle ArbitraryCrossing


isAlternating :: NonAlternatingTangle -> Bool
isAlternating =
	let altOrBorderEdge d = isLeg (opposite d) || passOver d == passUnder (opposite d)
	in all altOrBorderEdge . allDarts


invertCrossings :: NonAlternatingTangle -> NonAlternatingTangle
invertCrossings = mapCrossingStates (alterCrossingOrientation (c <*>))


{-
crossingWrithe :: Dart ArbitraryCrossing -> Dart ArbitraryCrossing -> Int
crossingWrithe a b
	| a == nextCCW b  = d
	| a == nextCW b   = -d
	| otherwise       = error "crossingWrithe: something strange"
	where
		d = if passOver b then 1 else -1


threadWrithe :: [(Dart ArbitraryCrossing, Dart ArbitraryCrossing)] -> Int
threadWrithe = fst . foldl' edgeWrithe (0, Map.empty)
	where
		edgeWrithe (writhe, m) (d, _)
			| isLeg d          = (writhe, m)
			| Map.member cr m  = (writhe + crossingWrithe cr ((Map.!) m cr) d, m)
			| otherwise        = (writhe, Map.insert cr d m)
			where
				cr = incidentCrossing d


selfWrithe :: NonAlternatingTangle -> Int
selfWrithe = sum . map threadWrithe . allThreads


linkingNumber :: NonAlternatingTangle -> [Int]
linkingNumber tangle = sort $ map abs $ concatMap threadLinkings threads
	where
		threads = zip (allThreads tangle) [1 ..]

		n = length threads

		threadId = Array.array (dartsRange tangle) $ z ++ concatMap threadNum threads
			where
				z = zip (allDarts tangle) (repeat (0 :: Int))
				threadNum (thread, i) = zip (filter isDart $ map snd thread) (repeat i)

		threadLinkings (thread, i) =
			runST $ do
				ln <- newArray (1, n) 0 :: ST s (STUArray s Int Int)
				forM_ (filter isDart $ map snd thread) $ \ d -> do
					let (dl, j) = linking d
					when (i /= j) $ do
						cl <- readArray ln j
						writeArray ln j (cl + dl)
				mapM (readArray ln) [1 .. (i - 1)]
			where
				linking d
					| l > 0      = (p, l)
					| r > 0      = (-p, r)
					| otherwise  = error "no thread"
					where
						p = if passOver d then 1 else -1
						l = threadId ! nextCCW d
						r = threadId ! nextCW d
-}