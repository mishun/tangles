module Main (main) where

import System.CPUTime (getCPUTime)
import Control.Monad.State (execState, modify)
import Control.Monad (forM_)
import Text.Printf
import Diagrams.Prelude
import Math.Combinatorics.ChordDiagram
import Math.Combinatorics.ChordDiagram.Draw
import TestUtil.Drawing


main :: IO ()
main = do
    putStrLn "n\t#\ttime\tdiagrams/s"
    forM_ [1 .. 10] $ \ !n -> do
        beginTime <- getCPUTime
        let total :: Int
            total = countChordDiagrams (generateNonPlanarRaw n)
        total `seq` return ()
        endTime <- getCPUTime
        let time = (fromInteger (endTime - beginTime) :: Double) / 1.0e12
        putStrLn $ printf "%i\t%i\t%f\t%f" n total time (realToFrac total / time)

        writeSVGImage "cd.svg" (Width 500) $ pad 1.05 $ flip execState mempty $
            forM_ (listChordDiagrams $ generateNonPlanarRaw 4) $ \ (cd, _) ->
                modify (=== pad 1.1 (drawCDInsideCircleDef cd))
