module Graphics.HP.Image
	(
	  Image(..)

	, stroke
	, stroke_
	, fill
	, fill_
	, also
	, image
	, transformed
	, drawOptions
	, appendTransform
	, setTransform
	) where

import qualified Control.Monad as Monad
import Graphics.HP.ImageBody
import Graphics.HP.Path
import Graphics.HP.DrawContext
import Graphics.HP.Transform


data Image v = Image ImageBody v deriving (Show)

instance Monad.Functor Image where
	fmap f (Image img v) = Image img $ f v


instance Monad.Monad Image where
	(>>=) (Image imageA a) f = Image (mergeImages imageA imageB) b
		where
			(Image imageB b) = f a

	(>>) (Image imageA _) (Image imageB b) = Image (mergeImages imageA imageB) b

	return v = Image nullImage v

	fail msg = error msg


stroke :: [DrawContext] -> Path -> Image ()
stroke con path = also con $ stroke_ path


stroke_ :: Path -> Image ()
stroke_ path = Image (Stroke path) ()


fill :: [DrawContext] -> Path -> Image ()
fill con path = also con $ fill_ path


fill_ :: Path -> Image ()
fill_ path = Image (Fill path) ()


also :: [DrawContext] -> Image () -> Image ()
also con (Image img ()) = Image (makeAlso (aggregateContexts con) identity img) ()


image :: Image () -> Image ()
image (Image img ()) = Image (makeAlso emptyContext identity img) ()


transformed :: [Transform] -> Image () -> Image ()
transformed transList (Image img ()) = Image (makeAlso emptyContext trans img) ()
	where
		trans = transform transList


drawOptions :: [DrawContext] -> Image ()
drawOptions con = Image (DrawOptions (aggregateContexts con) (False, identity)) ()


appendTransform :: [Transform] -> Image ()
appendTransform trList = Image (DrawOptions emptyContext (False, transform trList)) ()


setTransform :: [Transform] -> Image ()
setTransform trList = Image (DrawOptions emptyContext (True, transform trList)) ()
