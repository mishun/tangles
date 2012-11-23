module Main (main) where

import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.State.Strict (State, execState, get, put)
import Data.List (intercalate, foldl')
import System.CPUTime (getCPUTime)
import Text.Printf
import Control.Monad
import Math.Algebra.Group.Dn (DnSubGroup, hasReflectionPart, rotationPeriod)
import Math.KnotTh.Crossings.SubTangle
import Math.KnotTh.Knotted
import Math.KnotTh.Tangles.Projection
import Math.KnotTh.Tangles.Draw
import Math.KnotTh.Tangles.BorderIncremental.IncrementalGluing
import Math.KnotTh.Tangles.Generation.FlypeGenerator
import Graphics.HP


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


main :: IO ()
main = do
	printTable "Prime projections" False (simpleIncrementalGenerator primeProjectionType [ProjectionCrossing]) 8
	printTable "Template projections" False (simpleIncrementalGenerator templateProjectionType [ProjectionCrossing]) 9
	printTable "Alternating tangles" False generateFlypeEquivalent 8

	writePostScriptFile "TestTangleGenerator.ps" $ do
		let a4Width = 595
		let a4Height = 842

		transformed [shifted (0.2 * a4Width, 0.98 * a4Height), scaled 6] $
			generateFlypeEquivalentDecomposition 5 $ \ template _ -> do
				let tangle = substitute template
				when (numberOfCrossings tangle == 5 && numberOfLegs tangle == 4) $ do
					drawTangle 0.01 tangle
					transformed [shifted (3, 0)] $ drawTangle 0.01 $ tangleProjection template
					appendTransform [shifted (0, -2.2)]
