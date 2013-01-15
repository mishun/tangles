module Graphics.HP.PostScript.Transform
    ( transformToPS
    ) where

import qualified Data.List as List
import Graphics.HP.Transform


transformToPS :: Transform -> String
transformToPS t = psCode
    where
        dx = xPart t
        dy = yPart t

        xx = xxPart t
        xy = xyPart t
        yx = yxPart t
        yy = yyPart t

        norm = sqrt $ (xx * xx) + (xy * xy) + (yx * yx) + (yy * yy)
        eps = 1e-8

        eq a b = abs ((a - b) / norm) < eps

        hasTranslate = (abs dx > 0.0) || (abs dy > 0.0)

        isScale = eq 0.0 xy && eq 0.0 yx

        isIdentity = isScale && eq 1.0 xx && eq 1.0 yy

        isRotation = eq xx yy && eq (-xy) (yx) && eq 1.0 (xx * xx + xy * xy)

        psCode
            | hasTranslate  = if isIdentity then translate else general
            | isIdentity    = ""
            | isScale       = scale
            | isRotation    = rotate
            | otherwise     = general

        general =
            let list = [xx, yx, xy, yy, dx, dy]
            in "[" ++ (List.intercalate " " $ map show list) ++ "] concat"

        translate = List.concat [show dx, " ", show dy, " translate"]

        scale = List.concat [show xx, " ", show yy, " scale"]

        rotate =
            let angle = 180.0 * (atan2 yx xx) / pi
            in (show angle) ++ " rotate"
