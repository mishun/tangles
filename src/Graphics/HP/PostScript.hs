module Graphics.HP.PostScript
    ( writePostScriptFile
    , toPostScript
    ) where

import qualified Data.List as List
import qualified System.IO as IO
import Graphics.HP.DrawContext
import Graphics.HP.Image
import Graphics.HP.ImageBody
import Graphics.HP.Transform
import Graphics.HP.PostScript.Transform
import Graphics.HP.PostScript.Context
import Graphics.HP.PostScript.Path


writePostScriptFile :: String -> Image () -> IO.IO ()
writePostScriptFile fileName img = do
    file <- IO.openFile fileName IO.WriteMode
    mapM_ (IO.hPutStr file) [header, toPostScript img, eof]
    IO.hClose file
    return ()

    where
        header = "%!PS-Adobe-2.0\n%%EndComments\n\n"
        eof = "%%Trailer\n%%EOF\n"


toPostScript :: Image () -> String
toPostScript (Image img ()) = traverse gen img
    where
        gen = CodeGenerator strokeCode fillCode

        strokeCode (gc, lc) (gt, lt) path = psWithContext (aggregateContexts [gc, lc], transform [gt, lt]) $ (pathToPS path) ++ " stroke\n"

        fillCode (gc, lc) (gt, lt) path = psWithContext (aggregateContexts [gc, lc], transform [gt, lt]) $ (pathToPS path) ++ " fill\n"


psWithContext :: (DrawContext, Transform) -> String -> String
psWithContext (con, trans) psCode
    | null psCode       = ""
    | null contextCode  = psCode ++ "\n"
    | otherwise         = List.concat ["gsave\n", contextCode, "\n", psCode, "grestore\n"]

    where
        contextCode = unlines $ filter (not . null) [contextToPS con, transformToPS trans]
