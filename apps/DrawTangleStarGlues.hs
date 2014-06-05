module Main where

import Data.Ord (comparing)
import Data.List (sortBy)
import qualified Data.Map as M
import Control.Monad.State.Strict (execState, modify)
import Control.Monad (guard)
import Text.Printf
import System.Environment (getArgs)
import Diagrams.Prelude
import Diagrams.Backend.Cairo
import qualified Math.Algebra.Group.Dn as Dn
import Math.Combinatorics.ChordDiagram (generateNonPlanarRaw, listChordDiagrams, genusOfChordDiagram)
import Math.Combinatorics.ChordDiagram.Draw (drawCDInsideCircleDef)
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.Construction
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Tabulation.TangleDiagramsCascade
import Math.Topology.KnotTh.Draw


main :: IO ()
main = do
    [targetGenus, maxN] <- fmap (map read) getArgs

    let makeDiagrams tangleGenerator =
            let diagrams = map (listChordDiagrams . generateNonPlanarRaw) [0 ..]
                merge (a, al) (_, bl) = (a, sortBy (comparing $ numberOfLegs . fst) (al ++ bl))
            in M.elems $ M.fromListWith merge $ do
                (tangle, tangleSymmetry) <- execState (tangleGenerator $ \ (!tangle, (!symmetry, _)) -> modify ((tangle, symmetry) :)) []

                let l = numberOfLegs tangle
                (cd, (starMirror, starPeriod)) <- diagrams !! (l `div` 2)
                guard $ targetGenus == genusOfChordDiagram cd

                rot <- [0 .. gcd starPeriod (Dn.rotationPeriod tangleSymmetry) - 1]
                mir <- if not starMirror && not (Dn.hasReflectionPart tangleSymmetry)
                           then [False, True]
                           else [False]
                let g = Dn.fromReflectionRotation l (mir, rot)
                    link = fromTangleAndStar cd $ transformTangle g tangle
                return (unrootedHomeomorphismInvariant link, (link, [(transformTangle g tangle, cd)]))

    renderCairo (printf "TangleStarGlues-%i-%i.svg" targetGenus maxN) (Width 512) $ pad 1.05 $
        vcat' with { _sep = 0.8 } $ do
            (link, gluings) <- makeDiagrams (forCCP_ $ primeProjections maxN)
            return $ hcat' with { _sep = 0.8 } $ ((drawKnotDef link ||| strutX 1) :) $ do
                (tangle, cd) <- gluings
                return $ drawKnotDef tangle ||| strutX 0.2 ||| drawCDInsideCircleDef cd
