module Graphics.HP.DrawContext
    ( DrawContext
    , DashType(..)

    , drawColor
    , drawLineWidth
    , dashType

    , emptyContext
    , defaultContext
    , withColor
    , withLineWidth
    , dashedEvenly

    , aggregateContexts
    ) where

import Data.Maybe
import Graphics.HP.Color


data DashType = DashedSolid | DashedEvenly deriving (Eq, Show)

data DrawContext =
    Context
    {
        drawColor     :: Maybe Color,
        drawLineWidth :: Maybe (Double, Bool),
        dashType      :: Maybe DashType
    }
    deriving (Eq, Show)


emptyContext :: DrawContext
emptyContext = Context Nothing Nothing Nothing


defaultContext :: DrawContext
defaultContext = Context (Just black) (Just (0.0, False)) (Just DashedSolid)


withColor :: Color -> DrawContext
withColor color = Context (Just color) Nothing Nothing


withLineWidth :: Double -> DrawContext
withLineWidth width = Context Nothing (Just $ (max 0.0 width, False)) Nothing


dashedEvenly :: DrawContext
dashedEvenly = Context Nothing Nothing (Just DashedEvenly)


aggregateContexts :: [DrawContext] -> DrawContext
aggregateContexts = foldr contextsSum emptyContext


contextsSum :: DrawContext -> DrawContext -> DrawContext
contextsSum a b = Context color width dash
    where
        color = sumWith drawColor
        width = sumWith drawLineWidth
        dash = sumWith dashType

        sumWith f =
            if isJust fb
                then fb
                else fa

            where
                fa = f a
                fb = f b
