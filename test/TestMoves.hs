module Main (main) where

import Control.Monad (forM_)
import Text.Printf
import Diagrams.Prelude
import Math.Topology.KnotTh.Moves.Test
import TestUtil.Drawing


main :: IO ()
main =
    forM_ testMovesPictures $ \ (pictureName, picture) -> do
        let fileName = printf "TextMoves-%s.svg" pictureName
        putStrLn fileName
        writeSVGImage fileName (Width 250) $ pad 1.05 picture
