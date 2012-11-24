module TestTangles.PrintTable
	( printTable
	) where

import Data.Maybe (isJust, fromJust)
import Data.List (intercalate, foldl')
import qualified Data.Map as Map
import Control.Monad.State.Strict (State, execState, get, put)
import Text.Printf (printf)
import System.CPUTime (getCPUTime)
import Math.Algebra.Group.Dn (DnSubGroup, hasReflectionPart, rotationPeriod)
import Math.KnotTh.Tangles


printTable :: String -> Bool -> (Int -> (Tangle ct -> DnSubGroup -> State (Map.Map (Int, Int) Int) ()) -> State (Map.Map (Int, Int) Int) ()) -> Int -> IO ()
printTable name isLabelled generator maxN = do
	putStrLn $ name ++ ":"
	beginTime <- getCPUTime

	let table =
		let yield !tangle !symmetry = do
			let n = numberOfCrossings tangle
			let l = numberOfLegs tangle
			let weight
				| isLabelled  = rotationPeriod symmetry * (if hasReflectionPart symmetry then 1 else 2)
				| otherwise   = 1
			let update !k = Just $! maybe weight (+ weight) k
			get >>= (\ !m -> n `seq` l `seq` put $! Map.alter update (n, l) m)
		in execState (generator maxN yield) (Map.empty :: Map.Map (Int, Int) Int)

	let totalTangles = sum $ map snd $ Map.assocs table

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
