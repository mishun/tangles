module Main where

import System.Environment (getArgs)
import Diagrams.Prelude
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.Draw
import TestUtil.Drawing


main :: IO ()
main = do
    [file, knotType, representation] <- getArgs
    writeSVGImage file (Width 500) $ pad 1.05 $
        case knotType of
            "EmbeddedLinkDiagram"    -> drawKnotDef (implode (read representation) :: EmbeddedLinkDiagram)
            "EmbeddedLinkProjection" -> drawKnotDef (implode (read representation) :: EmbeddedLinkProjection)
            "LinkDiagram"            -> drawKnotDef (implode (read representation) :: LinkDiagram)
            "LinkProjection"         -> drawKnotDef (implode (read representation) :: LinkProjection)
            "TangleDiagram"          -> drawKnotDef (implode (read representation) :: TangleDiagram)
            "TangleProjection"       -> drawKnotDef (implode (read representation) :: TangleProjection)
            _                        -> error "unknown knot type"

