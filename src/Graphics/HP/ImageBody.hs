module Graphics.HP.ImageBody
	(
	  ImageBody(DrawOptions, Stroke, Fill)
	, nullImage
	, isNullImage
	, makeAlso
	, mergeImages

	, CodeGenerator(..)
	, traverse
	) where

import qualified Data.List as List
import Graphics.HP.Path
import Graphics.HP.DrawContext
import Graphics.HP.Transform


data ImageBody = MultiImage [ImageBody]
	| DrawOptions DrawContext (Bool, Transform)
	| Also DrawContext Transform ImageBody
	| Stroke Path
	| Fill Path
	deriving (Show)


nullImage :: ImageBody
nullImage = MultiImage []


isNullImage :: ImageBody -> Bool
isNullImage img = case img of
	(MultiImage list) -> List.all isNullImage list
	(Also _ _ image) -> isNullImage image
	_ -> False


makeAlso :: DrawContext -> Transform -> ImageBody -> ImageBody
makeAlso context trans image
	| isNullImage image  = nullImage
	| otherwise          = Also context trans image


mergeImages :: ImageBody -> ImageBody -> ImageBody
mergeImages a b
	| isNullImage a  = b
	| isNullImage b  = a
	| otherwise      =
		case b of
			(MultiImage listB) ->
				case a of
					(MultiImage listA) -> MultiImage (listA ++ listB)
					_                  -> MultiImage (a : listB)

			_ ->
				case a of
					(MultiImage listA) -> MultiImage (listA ++ [b])
					_                  -> MultiImage [a, b]



data CodeGenerator = CodeGenerator
	{
--		codeGenBegin, codeGenEnd ::
		codeGenStroke, codeGenFill :: (DrawContext, DrawContext) -> (Transform, Transform) -> Path -> String
	}


traverse :: CodeGenerator -> ImageBody -> String
traverse generator root = dfs defaultContext identity root
	where
		dfs gc gt (MultiImage images) = List.concat codes
			where
				imgContext = gc--aggregateContexts [gc]

				imgTransform = gt--transform [gt]

				codes = snd $ List.mapAccumL accum (emptyContext, identity) images

				accum curData@(curContext, curTransform) img =
					case img of
						(DrawOptions newContext (absolute, newTransform)) ->
							let
								resContext = aggregateContexts [curContext, newContext]
								resTransform =
									if absolute
										then newTransform
										else transform [curTransform, newTransform]
							in ((resContext, resTransform), "")

						_ ->
							(curData, dfs (aggregateContexts [imgContext, curContext]) (transform [imgTransform, curTransform]) img)

		dfs _ _ (DrawOptions _ _) = ""

		dfs gc gt (Also context trans image) =
			dfs (aggregateContexts [gc, context]) (transform [gt, trans]) image

		dfs gc gt (Stroke path) = (codeGenStroke generator) (gc, emptyContext) (gt, identity) path
		dfs gc gt (Fill path) = (codeGenFill generator) (gc, emptyContext) (gt, identity) path
