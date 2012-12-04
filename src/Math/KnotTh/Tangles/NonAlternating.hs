module Math.KnotTh.Tangles.NonAlternating
	( module Math.KnotTh.Crossings.Arbitrary
	, module Math.KnotTh.Tangles
	, NonAlternatingTangle
	, isAlternating
	, alternatingDefect
	, selfWrithe
	, linkingNumbers
	) where

import qualified Data.Map as Map
import Data.List (foldl', sort)
import Data.Array (array, (!))
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (when, forM_)
import Math.KnotTh.Crossings.Arbitrary
import Math.KnotTh.Tangles


type NonAlternatingTangle = Tangle ArbitraryCrossing


isAlternating :: NonAlternatingTangle -> Bool
isAlternating = (== 0) . alternatingDefect


alternatingDefect :: NonAlternatingTangle -> Int
alternatingDefect tangle =
	let defect a
		| isDart b && passOver a == passOver b  = 1
		| otherwise                             = 0
		where
			b = opposite a
	in (sum $ map defect $ allDarts tangle) `div` 2


selfWrithe :: NonAlternatingTangle -> Int
selfWrithe =
	let threadWrithe =
		let edgeWrithe (!w, !m) (!d, _)
			| isLeg d          = (w, m)
			| Map.member cr m  = (w + writhe (m Map.! cr) d, m)
			| otherwise        = (w, Map.insert cr d m)
			where
				cr = incidentCrossing d
		in fst . foldl' edgeWrithe (0, Map.empty)
	in sum . map threadWrithe . allThreads


linkingNumbers :: NonAlternatingTangle -> [Int]
linkingNumbers tangle = sort $ map abs $ concatMap threadLinkings threads
	where
		threads = zip (allThreads tangle) [1 ..]

		n = length threads

		threadId = array (0, 2 * numberOfEdges tangle - 1) $ map (\ (d, x) -> (dartArrIndex d, x)) $ z ++ concatMap threadNum threads
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
						l = threadId ! dartArrIndex (nextCCW d)
						r = threadId ! dartArrIndex (nextCW d)
