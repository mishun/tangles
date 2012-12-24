module Math.KnotTh.Link.GaussCode
	( fromGaussCode
	, toGaussCode
	, fromDTCode
	, toDTCode
	) where

import qualified Data.Map as M
import Data.STRef (newSTRef, readSTRef, writeSTRef)
import Data.Array.ST (STArray, STUArray, newArray, newArray_, readArray, writeArray, getElems)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM, when, unless)
import Math.KnotTh.Link.NonAlternating


fromGaussCode :: [[Int]] -> NonAlternatingLink
fromGaussCode = decode . simplifyGaussCode


toGaussCode :: NonAlternatingLink -> [[Int]]
toGaussCode link = flip map (allThreads link) $
	map $ \ (_, d) ->
		(crossingIndex $ incidentCrossing d) * (if passOver d then 1 else -1)


fromDTCode :: [Int] -> NonAlternatingLink
fromDTCode _ = undefined


toDTCode :: NonAlternatingLink -> [Int]
toDTCode _ = undefined


decode :: (CrossingType ct) => (Int, [[Int]], [CrossingState ct]) -> Link ct
decode (n, threads, states) = runST $ do
	let chord = foldr (\ i t -> t) threads [1 .. n]
	let incidence = []
	return $! fromList (length $ filter null threads, zip incidence states)


simplifyGaussCode :: [[Int]] -> (Int, [[Int]], [ArbitraryCrossingState])
simplifyGaussCode code = runST $ do
	let n = sum $ flip map code $ \ x ->
		let n2 = length x
		in if even n2
			then n2 `div` 2
			else error "fromGaussCode: lengths must be even"

	index <- do
		free <- newSTRef 1
		indices <- newSTRef $ M.empty
		return $! \ !x -> do
			m <- readSTRef indices
			if M.member x m
				then return $! m M.! x
				else do
					y <- readSTRef free
					when (y > n) $ fail "fromGaussCode: too many different values in gauss code"
					writeSTRef free $! y + 1
					writeSTRef indices $! M.insert x y m
					return $! y

	states <- newArray_ (1, n) :: ST s (STArray s Int ArbitraryCrossingState)
	visitedP <- newArray (1, n) False :: ST s (STUArray s Int Bool)
	visitedN <- newArray (1, n) False :: ST s (STUArray s Int Bool)

	let pickV x
		| x > 0      = visitedP
		| x < 0      = visitedN
		| otherwise  = error "fromGaussCode: zero presented"

	simplified <-
		forM code $ mapM $ \ !raw -> do
			i <- index $ abs raw

			readArray (pickV raw) i >>= \ v ->
				when v $ fail $ "fromGaussCode: duplication on " ++ show raw
			writeArray (pickV raw) i True

			second <- readArray (pickV (-raw)) i
			unless second $
				writeArray states i $ if raw > 0
					then overCrossing
					else underCrossing

			return $! i

	states' <- getElems states
	return $! (n, simplified, states')
