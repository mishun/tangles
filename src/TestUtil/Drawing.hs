module TestUtil.Drawing
    ( writeSVGImage
    , writeEPSImage
    ) where

import Diagrams.Prelude (R2, Diagram, SizeSpec2D, renderDia)
import Diagrams.Backend.SVG (SVG(..), Options(..))
import Diagrams.Backend.Postscript (Postscript(..), OutputFormat(EPS), Options(PostscriptOptions))
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Data.ByteString.Lazy as BS


writeSVGImage :: String -> SizeSpec2D -> Diagram SVG R2 -> IO ()
writeSVGImage fileName dim =
    BS.writeFile fileName . renderSvg .
        renderDia SVG
            SVGOptions
                { _size           = dim
                , _svgDefinitions = Nothing
                }


writeEPSImage :: String -> SizeSpec2D -> Diagram Postscript R2 -> IO ()
writeEPSImage fileName dim =
    renderDia Postscript $ PostscriptOptions fileName dim EPS
