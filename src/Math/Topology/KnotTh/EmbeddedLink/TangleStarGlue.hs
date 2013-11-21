{-# LANGUAGE Rank2Types #-}
module Math.Topology.KnotTh.EmbeddedLink.TangleStarGlue
    ( StarType(..)
    , tangleStarGlue
    ) where

import qualified Data.Set as S
import Control.Monad.State.Strict (lift, evalStateT, gets, modify)
import Control.Monad (when, forM_)
import qualified Math.Algebra.Group.Dn as Dn
import Math.Combinatorics.ChordDiagram (generateNonPlanarRaw, generateBicolourableNonPlanarRaw, listChordDiagrams)
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.EmbeddedLink
import Math.Topology.KnotTh.EmbeddedLink.Construction (fromTangleAndStar)
import Math.Topology.KnotTh.EmbeddedLink.IsomorphismTest


data StarType = BicolourableStar | AnyStar


tangleStarGlue :: (Monad m, CrossingType ct)
                   => StarType
                   -> (forall m'. (Monad m') => ((Tangle ct, Dn.DnSubGroup) -> m' ()) -> m' ())
                   -> (EmbeddedLink ct -> m ())
                   -> m ()

tangleStarGlue starType tangleGenerator yield =
    let generator =
            case starType of
                BicolourableStar -> generateBicolourableNonPlanarRaw
                AnyStar          -> generateNonPlanarRaw

    in flip evalStateT S.empty $
        tangleGenerator $ \ (!tangle, !tangleSymmetry) ->
            let l = numberOfLegs tangle
            in forM_ (listChordDiagrams $ generator (l `div` 2)) $ \ (!star, (!starMirror, !starPeriod)) ->
                let variants = do
                        rot <- [0 .. gcd starPeriod (Dn.rotationPeriod tangleSymmetry) - 1]
                        mir <- if not starMirror && not (Dn.hasReflectionPart tangleSymmetry)
                                   then [False, True]
                                   else [False]
                        return $! Dn.fromReflectionRotation l (mir, rot)

                in forM_ variants $ \ !g -> do
                    let link = fromTangleAndStar star $ transformTangle g tangle
                        token = isomorphismTest link
                    new <- gets (S.notMember token)
                    when new $ do
                        modify (S.insert token)
                        lift $ yield link
