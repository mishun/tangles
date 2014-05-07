module Main (main) where

import System.CPUTime (getCPUTime)
import Control.Monad (when, forM_)
import System.Environment (getArgs)
import Text.Printf
import Diagrams.Prelude
import Math.Combinatorics.ChordDiagram
import Math.Combinatorics.ChordDiagram.Draw
import TestUtil.Drawing


main :: IO ()
main = do
    (draw, maxN) <- do
        args <- getArgs
        return $ case args of
            [maxN]         -> (False, read maxN)
            ["draw", maxN] -> (True, read maxN)
            _              -> error "bad args"

    putStrLn "n\t#\ttime\tdiagrams/s"
    forM_ [1 .. maxN] $ \ n -> do
        beginTime <- getCPUTime
        let total :: Int
            total = countChordDiagrams (generateNonPlanarRaw n)
        total `seq` return ()
        endTime <- getCPUTime
        let time = (fromInteger (endTime - beginTime) :: Double) / 1.0e12
        putStrLn $ printf "%i\t%i\t%f\t%f" n total time (realToFrac total / time)

        when draw $
            writeSVGImage (printf "chord-diagrams-%i.svg" n) (Width 500) $ pad 1.05 $
                vcat' with { _sep = 0.2 } $ do
                    (cd, _) <- listChordDiagrams $ generateNonPlanarRaw n
                    return $ drawCDInsideCircleDef cd
