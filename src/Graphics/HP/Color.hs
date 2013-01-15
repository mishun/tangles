module Graphics.HP.Color
    ( Color
    , toRGB
    , fromRGB
    , interpolate

    , white
    , black
    , red
    , green
    , blue
    , magenta
    , yellow
    , lightBlue
    , orange
    , purple
    , brown
    , darkGreen
    , grey
    ) where


data Color = Color Double Double Double deriving (Eq, Show)


toRGB :: Color -> (Double, Double, Double)
toRGB (Color r g b) = (r, g, b)


fromRGB :: (Double, Double, Double) -> Color
fromRGB (r, g, b) = Color (lim r) (lim g) (lim b)
    where
        lim = (min 1.0) . (max 0.0)


interpolate :: Double -> (Color, Color) -> Color
interpolate coef (aColor, bColor) = fromRGB $ (mix ar br, mix ag bg, mix ab bb)
    where
        mix ac bc = a * ac + b * bc
            where
                a = min 1.0 $ max 0.0 coef
                b = 1.0 - a

        (ar, ag, ab) = toRGB aColor
        (br, bg, bb) = toRGB bColor


white :: Color
white = fromRGB (1.0, 1.0, 1.0)

black :: Color
black = fromRGB (0.0, 0.0, 0.0)

red :: Color
red = fromRGB (1.0, 0.0, 0.0)

green :: Color
green = fromRGB (0.0, 1.0, 0.0)

blue :: Color
blue = fromRGB (0.0, 0.0, 1.0)

magenta :: Color
magenta = fromRGB (1.0, 0.0, 1.0)

yellow :: Color
yellow = fromRGB (1.0, 1.0, 0.0)

lightBlue :: Color
lightBlue = fromRGB (0.0, 1.0, 1.0)

orange :: Color
orange = fromRGB (1.0, 0.7, 0.0)

purple :: Color
purple = fromRGB (0.7, 0.3, 1.0)

brown :: Color
brown = fromRGB (0.7, 0.3, 0.0)

darkGreen :: Color
darkGreen = fromRGB (0.0, 0.5, 0.0)

grey :: Double -> Color
grey c = fromRGB (c, c, c)
