{-# LANGUAGE Rank2Types #-}
module TestTangles.Table
	( generateTable
	, generateTable'
	, printTable
	) where

import Data.Maybe (isJust, fromJust)
import Data.List (intercalate, foldl')
import qualified Data.Map as Map
import Control.Monad.State.Strict (execState, get, put)
import Text.Printf (printf)
import System.CPUTime (getCPUTime)
import Math.Algebra.Group.Dn (DnSubGroup, hasReflectionPart, rotationPeriod)
import Math.KnotTh.Tangle


generateTable :: Bool -> (forall m. (Monad m) => (Tangle ct -> DnSubGroup -> m ()) -> m ()) -> Map.Map (Int, Int) Int
generateTable isLabelled generator =
	let yield !tangle !symmetry = do
		let weight
			| isLabelled  = rotationPeriod symmetry * (if hasReflectionPart symmetry then 1 else 2)
			| otherwise   = 1
		get >>= (\ !m -> put $! Map.insertWith (+) (numberOfCrossings tangle, numberOfLegs tangle) weight m)
	in execState (generator yield) Map.empty


generateTable' :: (forall m. (Monad m) => (Tangle ct -> m ()) -> m ()) -> Map.Map (Int, Int) Int
generateTable' generator =
	let yield !tangle = get >>= (\ !m -> put $! Map.insertWith (+) (numberOfCrossings tangle, numberOfLegs tangle) 1 m)
	in execState (generator yield) Map.empty


printTable :: String -> Map.Map (Int, Int) Int -> IO ()
printTable name table = do
	beginTime <- getCPUTime
	putStrLn $ name ++ ":"
	let maxN = maximum $ map fst $ Map.keys table
	let totalTangles = sum $ Map.elems table
	putStr $
		let	possibleLegs = [ 2 * (i + 1) | i <- [0 .. maxN] ]

			total = "total: " ++ show totalTangles
			header = intercalate "\t" $ "l\\n" : (map show [1 .. maxN])
			line l = intercalate "\t" $ (show l) : (map (\ c -> cell (c, l)) [1 .. maxN])

			allCr =
				let numForCr c = foldl' (\ !carry ((!cc, !_), !cn) -> carry + if cc == c then cn else 0) 0 $ Map.assocs table
				in intercalate "\t" $ "all:" : (map (show . numForCr) [1 .. maxN])

			cell arg
				| isJust r && x > 0  = show x
				| otherwise          = "."
				where
					r = Map.lookup arg table
					x = fromJust r
		in unlines $ [header] ++ (map line possibleLegs) ++ [allCr, total]
	endTime <- getCPUTime
	let time = ((fromInteger (endTime - beginTime)) :: Double) / 1.0e12
	printf "Time = %fs (%f tangles/s)\n" time (realToFrac totalTangles / time)
