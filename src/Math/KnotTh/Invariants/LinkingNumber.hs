module Math.KnotTh.Invariants.LinkingNumber
	( linkingNumbersOfTangle
	) where

import Data.List (sort)
import Data.Array ((!), array)
import Data.Array.ST (STUArray, newArray, readArray, writeArray)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_, when)
import Math.KnotTh.Tangle.NonAlternating


linkingNumbersOfTangle :: NonAlternatingTangle -> [Int]
linkingNumbersOfTangle tangle = sort $ map abs $ concatMap threadLinkings threads
	where
		threads = zip (allThreads tangle) [1 ..]

		n = length threads

		threadId =
			let arr = array (dartIndexRange tangle) $ do
				(thread, i) <- threads
				(a, b) <- thread
				[(dartIndex a, i), (dartIndex b, 0)]
			in (arr !) . dartIndex

		threadLinkings (thread, i) = runST $ do
			let linking d
				| l > 0             = (p, l)
				| r > 0             = (-p, r)
				| otherwise         = error "no thread"
				where
					p | passOver d  = 1
					  | otherwise   = -1

					l = threadId $ nextCCW d
					r = threadId $ nextCW d

			ln <- newArray (1, n) 0 :: ST s (STUArray s Int Int)

			forM_ (filter isDart $ map snd thread) $ \ d ->
				when (isDart d) $ do
					let (dl, j) = linking d
					when (i /= j) $ do
						cl <- readArray ln j
						writeArray ln j (cl + dl)

			mapM (readArray ln) [1 .. (i - 1)]
