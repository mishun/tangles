module Main (main) where

import Control.Monad (forM_)
import Text.Printf
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Math.Topology.KnotTh.Moves.Test


main :: IO ()
main =
    forM_ testMovesPictures $ \ (pictureName, picture) -> do
        let fileName = printf "example-moves-%s.svg" pictureName
        putStrLn fileName
        renderSVG fileName (mkSizeSpec $ V2 (Just 256) Nothing) $ pad 1.05 picture
