module Main (main) where

import Data.Ord
import Data.Function (on)
import Data.Maybe (mapMaybe)
import Data.List (sortBy, groupBy)
import Control.Monad
import Text.Printf
import Math.KnotTh.Tangle.BorderIncremental.SimpleTypes
import Math.KnotTh.Enumeration.DiagramInfo.MinimalDiagramInfo
import Math.KnotTh.Enumeration.Applied.NonAlternatingTangles
import Math.KnotTh.Draw.DrawKnot
import Math.KnotTh.Link.FromTangle
import Math.KnotTh.Invariants.JonesPolynomial
import Math.KnotTh.Invariants.LinkingNumber
import Graphics.HP
import Tests.Table


main :: IO ()
main = do
	let diagrams n yield =
		simpleIncrementalGenerator
			(triangleBoundedType n primeIrreducibleDiagramType)
			[ArbitraryCrossing]
			n
			(\ t _ -> when (numberOfLegs t <= 4) $ yield t)

	printTable "Diagrams" $ generateTable' $ diagrams 5

	let tangles n k =
		let classes = tangleClasses (diagrams $ n + k) :: [MinimalDiagramInfo NonAlternatingTangle]
		in siftTangles $ filter ((<= n) . numberOfCrossings . representative) classes

	let sifted = tangles 8 0

	printTable "Tangles" $ generateTable' $ forM_ (mapMaybe maybePrimeDiagram $ singleRepresentativeClasses sifted)

	printf "Collision classes: %i" (length $ collisionClasses sifted)
	writePostScriptFile "collisions.ps" $ do
		let a4Width = 595
		let a4Height = 842
		transformed [shifted (0.05 * a4Width, 0.98 * a4Height), scaled 10] $ do
			forM_ (collisionClasses sifted) $ \ cc -> do
				forM_ (zip cc [0 ..]) $ \ (info, i) ->
					transformed [shifted (2.2 * i, 0)] $ drawKnot 0.01 $ representative info
				appendTransform [shifted (0, -2.2)]
