module TestUtil.Drawing
    ( writeSVGImage
    ) where

import Diagrams.Prelude
import Diagrams.Backend.SVG
import Text.Blaze.Svg.Renderer.Utf8 (renderSvg)
import qualified Data.ByteString.Lazy as BS


writeSVGImage :: String -> SizeSpec2D -> Diagram SVG R2 -> IO ()
writeSVGImage fileName dim =
    BS.writeFile fileName . renderSvg . renderDia SVG SVGOptions { size = dim }
