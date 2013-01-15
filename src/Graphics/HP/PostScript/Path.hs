module Graphics.HP.PostScript.Path
    ( pathToPS
    ) where

import Data.List as List
import Graphics.HP.Path


pathToPS :: Path -> String

pathToPS (SimplePath list) = "newpath " ++ (List.intercalate " " $ map putPoint $ zip points [(0 :: Int) ..])
    where
        putPoint ((x, y), i) =
            let command = if i == 0 then " moveto" else " lineto"
            in List.concat [show x, " ", show y, command]

        points = map (\ (CheckPoint p) -> p) list

pathToPS (ClosedPath path) = (pathToPS path) ++ " closepath"

pathToPS Circumference = "0 0 1 0 360 arc closepath"
