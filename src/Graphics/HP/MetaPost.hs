module Graphics.HP.MetaPost
	(
	  writeMetaPostFile
	, toMetaPost
	) where

import qualified Data.List as List
import qualified System.IO as IO
import Data.Maybe
import Text.Printf
import Graphics.HP.Color
import Graphics.HP.Image
import Graphics.HP.DrawContext
import Graphics.HP.ImageBody
import Graphics.HP.Transform
import Graphics.HP.Path


writeMetaPostFile :: String -> Image () -> IO.IO ()
writeMetaPostFile _ _ = do
	return ()


toMetaPost :: Image () -> String
toMetaPost (Image img ()) = traverse gen img
	where
		gen = CodeGenerator strokeCode fillCode

		strokeCode (gc, lc) (gt, lt) path =
			List.concat [
				"begingroup\n",
				"\ttransform t;\n",
				printf "\txpart t = %.4f;" (xPart t),
				printf " ypart t = %.4f;" (yPart t),
				printf " xxpart t = %.4f;" (xxPart t),
				printf " xypart t = %.4f;" (xyPart t),
				printf " yxpart t = %.4f;" (yxPart t),
				printf " yypart t = %.4f;\n" (yyPart t),
				"\tdraw image( draw (", pathToMP path, ") ", contextToMP c, "; ) transformed t;\n",
				"endgroup;\n"
				]

			where
				c = aggregateContexts [gc, lc]

				t = transform [gt, lt]

		fillCode (gc, lc) _ path =
			List.concat [
				"begingroup\n",
				"\ttransform t;\n",
				"\tfill image( fill (", pathToMP path, ") ", contextToMP c, "; );\n",
				"endgroup;\n"
				]

			where
				c = aggregateContexts [gc, lc]

pathToMP :: Path -> String
pathToMP (SimplePath list) = List.intercalate " -- " $ map (\ p -> let (CheckPoint (x, y)) = p in printf "(%.4f, %.4f)" x y) list
pathToMP (ClosedPath path) = (pathToMP path) ++ " -- cycle"
pathToMP Circumference = "fullcircle scaled 2"


contextToMP :: DrawContext -> String
contextToMP context = List.intercalate " " $ filter (not . null) [dash, width, color]
	where
		width = printf "withpen (pencircle scaled %.4f)" (fst $ fromJust $ drawLineWidth context)

		color = let (r, g, b) = toRGB $ fromJust $ drawColor context
			in printf "withcolor (%.2f, %.2f, %.2f)" r g b

		dash = case (fromJust $ dashType context) of
			DashedSolid  -> ""
			DashedEvenly -> printf "dashed dashpattern (on %.4f off %.4f)" w w

			where
				w = 4 * (maybe 1.0 fst (drawLineWidth context))
