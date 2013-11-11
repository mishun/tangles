module Math.Topology.KnotTh.Invariants.Skein.StateModels.ChordDiagramsSum.ChordDiagramsSum
    ( ChordDiagram(..)
    , ChordDiagramsSum(..)
    , singletonStateSum
    , concatStateSums
    , mapStateSum
    , forAllSummands
    ) where

import Data.List (foldl', intercalate)
import qualified Data.Map as M
import Data.Array.IArray (bounds, elems)
import Data.Array.Unboxed (UArray)
import Control.Monad (forM_)
import Control.DeepSeq
import Control.Parallel.Strategies
import Text.Printf


data ChordDiagram a = ChordDiagram {-# UNPACK #-} !(UArray Int Int) !a deriving (Eq, Ord)


instance Functor ChordDiagram where
    fmap f (ChordDiagram p x) = ChordDiagram p $! f x


instance (NFData a) => NFData (ChordDiagram a) where
    rnf (ChordDiagram p x) = p `seq` rnf x


instance (Show a) => Show (ChordDiagram a) where
    show (ChordDiagram a x) =
        printf "(%s)%s" (show x) (show $ elems a)


data ChordDiagramsSum a = ChordDiagramsSum {-# UNPACK #-} !Int ![ChordDiagram a] deriving (Eq, Ord)


instance Functor ChordDiagramsSum where
    fmap f (ChordDiagramsSum order list) = ChordDiagramsSum order $ map (fmap f) list


instance (NFData a) => NFData (ChordDiagramsSum a) where
    rnf (ChordDiagramsSum _ list) = rnf list


instance (Show a) => Show (ChordDiagramsSum a) where
    show (ChordDiagramsSum _ list) =
        case list of
            [] -> "0"
            _  -> intercalate "+" $ map show list


singletonStateSum :: ChordDiagram a -> ChordDiagramsSum a
singletonStateSum summand @ (ChordDiagram a _) =
    ChordDiagramsSum (1 + snd (bounds a)) [summand]


concatStateSums :: (Eq a, Num a) => [ChordDiagramsSum a] -> ChordDiagramsSum a
concatStateSums [] = error $ printf "concatStateSum: empty"
concatStateSums list @ (ChordDiagramsSum order _ : _) =
    let s = map (\ (!k, !v) -> ChordDiagram k v) $
            filter ((/= 0) . snd) $! M.toList $
                foldl' (\ !m (ChordDiagram !k !v) -> M.insertWith' (+) k v m) M.empty $
                    concatMap (\ (ChordDiagramsSum order' list') ->
                            if order' == order
                                then list'
                                else error $ printf "concatStateSums: order conflict with %i and %i" order order'
                        ) list
    in ChordDiagramsSum order (s `using` evalList rseq)


mapStateSum :: (Eq a, Num a) => (ChordDiagram a -> ChordDiagramsSum a) -> ChordDiagramsSum a -> ChordDiagramsSum a
mapStateSum _ (ChordDiagramsSum order []) = ChordDiagramsSum order []
mapStateSum f (ChordDiagramsSum _ list) = concatStateSums $ map f list


forAllSummands :: (Monad m) => ChordDiagramsSum a -> (ChordDiagram a -> m ()) -> m ()
forAllSummands (ChordDiagramsSum _ list) = forM_ list
