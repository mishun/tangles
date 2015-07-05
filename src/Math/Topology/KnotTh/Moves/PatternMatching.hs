{-# LANGUAGE RankNTypes #-}
module Math.Topology.KnotTh.Moves.PatternMatching
    ( Pattern
    , PatternM
    , subTangleP
    , crossingP
    , connectionP
    , reconnectP
    , makePattern
    , searchMoves
    ) where

import Data.Maybe (mapMaybe)
import Data.List ((\\), subsequences)
import qualified Data.Set as S
import Data.Array.IArray ((!), (//), listArray)
import Data.Array.Unboxed (UArray)
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad.State (execState, gets, modify)
import Control.Monad (MonadPlus(..), unless, guard)
import Math.Topology.KnotTh.Tangle
import Math.Topology.KnotTh.Link
import Math.Topology.KnotTh.Moves.ModifyDSL


data PatternS a =
    PatternS ([Dart Tangle a] -> [Dart Tangle a]) (Tangle a) [Vertex Tangle a]


newtype PatternM s a x =
    PatternM
        { runPatternMatching :: PatternS a -> [(PatternS a, x)]
        }

newtype Pattern a x =
    Pattern
        { patternMatching :: Tangle a -> [x]
        }


instance Functor (PatternM s a) where
    fmap f x = PatternM (map (fmap f) . runPatternMatching x)


instance Applicative (PatternM s a) where
    pure = return

    (<*>) f' x' = do
        f <- f'
        x <- x'
        return $! f x


instance Monad (PatternM s a) where
    return x = PatternM (\ s -> [(s, x)])

    (>>=) val act = PatternM (concatMap (\ (s', x) -> runPatternMatching (act x) s') . runPatternMatching val)


instance Alternative (PatternM s a) where
    empty = PatternM (const [])

    (<|>) a b = PatternM (\ s -> runPatternMatching a s ++ runPatternMatching b s)


instance MonadPlus (PatternM s a) where
    mzero = empty

    mplus = (<|>)


subTangleP :: Int -> PatternM s a ([Dart Tangle a], [Vertex Tangle a])
subTangleP legs =
    PatternM $ \ (PatternS reorder tangle cs) -> do
        subList <- subsequences cs
        guard $ not $ null subList

        let sub :: UArray Int Bool
            sub = listArray (vertexIndicesRange tangle) (repeat False)
                    // map (\ d -> (vertexIndex d, True)) subList

        guard $
            let mask = execState (dfs $ head subList) S.empty
                dfs c = do
                    visited <- gets $ S.member c
                    unless visited $ do
                        modify $ S.insert c
                        mapM_ dfs $
                            filter ((sub !) . vertexIndex) $
                                mapMaybe endVertexM $ outcomingDarts c
            in all (`S.member` mask) subList

        let onBorder xy =
                let yx = opposite xy
                    x = beginVertex xy
                    y = beginVertex yx
                in isDart xy && (sub ! vertexIndex x) && (isLeg yx || not (sub ! vertexIndex y))

        let border = filter onBorder $ concatMap outcomingDarts cs
        guard $ legs == length border

        let borderCCW =
                let traverseNext = nextCW . opposite
                    restoreOutcoming out d | d == head border  = d : out
                                           | onBorder d        = restoreOutcoming (d : out) (opposite d)
                                           | otherwise         = restoreOutcoming out (traverseNext d)
                in restoreOutcoming [] (opposite $ head border)

        guard $ legs == length borderCCW

        i <- [0 .. legs - 1]
        return (PatternS reorder tangle (cs \\ subList), (reorder $ drop i borderCCW ++ take i borderCCW, subList))


crossingP :: PatternM s a ([Dart Tangle a], Vertex Tangle a)
crossingP =
    PatternM $ \ (PatternS reorder tangle cs) ->
        let try res _ [] = res
            try res skipped (cur : rest) =
                let next = PatternS reorder tangle (skipped ++ rest)
                    sh i = let od = outcomingDarts cur
                           in reorder $ drop i od ++ take i od
                in try ((next, (sh 0, cur)) : (next, (sh 1, cur)) : (next, (sh 2, cur)) : (next, (sh 3, cur)) : res) (cur : skipped) rest
        in try [] [] cs


connectionP :: [(Dart Tangle a, Dart Tangle a)] -> PatternM s a ()
connectionP = mapM_ (\ (a, b) -> guard (opposite a == b))


reconnectP :: (Show a) => (forall s. ModifyM Tangle a s ()) -> PatternM s' a (Tangle a)
reconnectP m =
    PatternM $ \ s@(PatternS _ tangle _) ->
        [(s, modifyKnot tangle m)]


makePattern :: Bool -> (forall s. PatternM s a x) -> Pattern a x
makePattern mirrored pattern =
    Pattern $ \ tangle -> do
        reorder <- id : [reverse | mirrored]
        let initial = PatternS reorder tangle (allVertices tangle)
        (_, res) <- runPatternMatching pattern initial
        return res


class (Knotted k) => PatternMatching k where
    searchMoves :: [Pattern a (Tangle a)] -> k a -> [k a]


instance PatternMatching Tangle where
    searchMoves patterns tangle = do
        pattern <- patterns
        patternMatching pattern tangle


instance PatternMatching Link where
    searchMoves patterns =
        map tangleToLink . searchMoves patterns . linkToTangle
