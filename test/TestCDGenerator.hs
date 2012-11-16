module Main (main) where

import qualified System.CPUTime as CPUTime
import Control.Monad
import Control.Monad.ST
import Text.Printf
import Math.Combinatorics.ChordDiagrams.Generator


main :: IO ()
main = do
	putStrLn "Chord diagrams generator"
	beginTime <- CPUTime.getCPUTime

	let target = [ 1, 0, 1, 2, 7, 29, 196, 1788, 21994 ]
	--let getCD !n = execState (generate n (\ _ -> get >>= \ !s -> put $! (s + 1))) 0
	forM_ [0 .. 10] $ \ i -> do
		let cd = runST $ generateNonPlanar i (\ c _ -> return $! c + 1) (0 :: Int)
		let msg
			| i >= length target  = "no data"
			| cd == target !! i   = "ok"
			| otherwise           = "failed"
		putStrLn $ printf "%2i: %i %s" i cd msg

	endTime <- CPUTime.getCPUTime
	putStrLn $ printf "Time = %fs" $ ((fromInteger (endTime - beginTime)) :: Float) / 1.0e12
	return ()
