module Main where

import System.Environment (getArgs)
import Diagrams.Prelude
import Diagrams.Backend.SVG
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.Draw


main :: IO ()
main = do
    [filePath, knotType, representation] <- getArgs
    renderSVG filePath (Width 512) $ pad 1.05 $
        case knotType of
            "EmbeddedLinkDiagram"    -> drawKnotDef (implode (read representation) :: EmbeddedLinkDiagram)
            "EmbeddedLinkProjection" -> drawKnotDef (implode (read representation) :: EmbeddedLinkProjection)
            "LinkDiagram"            -> drawKnotDef (implode (read representation) :: LinkDiagram)
            "LinkProjection"         -> drawKnotDef (implode (read representation) :: LinkProjection)
            "TangleDiagram"          -> drawKnotDef (implode (read representation) :: TangleDiagram)
            "TangleProjection"       -> drawKnotDef (implode (read representation) :: TangleProjection)
            _                        -> error "unknown knot type"
