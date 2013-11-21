{-# LANGUAGE RankNTypes #-}
module Math.Algebra.PlanarAlgebra.Reduction
    ( PlanarM
    , reduceWithStrategy
    , VertexM
    , DartM
    , oppositeM
    , nextCCWM
    , nextCWM
    ) where

import Data.Function (fix)
import Data.Array.IArray ((!), array, listArray)
import Data.Array.MArray (newArray_, readArray, writeArray)
import Data.Array (Array)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray)
import Data.STRef (STRef, newSTRef)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.ST (ST, runST)
import Control.Monad (forM_)
import Math.Algebra.PlanarAlgebra.Definition


type PlanarM s a = ReaderT (Context s a) (ST s)


data VertexM a =
    VertexM
        { degree    :: {-# UNPACK #-} !Int
        , incidence :: {-# UNPACK #-} !(UArray Int Int)
        , value     :: !a
        }

data DartM a =
    DartM
        { incidentVertex :: !(VertexM a)
        , dartPlace      :: {-# UNPACK #-} !Int
        }


data Context s a =
    Context
        { _opposite :: {-# UNPACK #-} !(STArray s Int (DartM a))
        , _free     :: {-# UNPACK #-} !(STRef s Int)
        }


data StrategyResult a = Return a | Contract (DartM a)

type Strategy a = forall s. [DartM a] -> PlanarM s a (StrategyResult a)


reduceWithStrategy :: (PlanarAlgebra a, PlanarState t) => a x -> (Vertex a x -> t) -> Strategy t -> t
reduceWithStrategy alg weight strategy =
    runST $ do
        context <- do
            let vs = (array (vertexIndicesRange alg) :: [(Int, a)] -> Array Int a) $
                    flip map (allVertices alg) $ \ v ->
                        let d = vertexDegree v
                            inc = listArray (0, d - 1) $ map dartIndex $ outcomingDarts v
                        in (vertexIndex v, VertexM d inc (weight v))

            opp <- newArray_ (dartIndicesRange alg)
            forM_ (allHalfEdges alg) $ \ d ->
                let (u, p) = endPair' d
                in writeArray opp (dartIndex d) (DartM (vs ! u) p)

            fi <- newSTRef $ 1 + snd (vertexIndicesRange alg)
            return Context { _opposite = opp, _free = fi }

        flip runReaderT context $
            fix $ \ continue -> do
                action <- strategy []
                case action of
                    Return r   -> return r
                    Contract _ -> continue


oppositeM :: DartM a -> PlanarM s a (DartM a)
oppositeM (DartM v p) =
    ask >>= \ context -> lift $
        readArray (_opposite context) (incidence v ! p)


nextCCWM :: DartM a -> DartM a
nextCCWM (DartM v p) =
    let d = degree v
    in DartM v ((p + 1) `mod` d)


nextCWM :: DartM a -> DartM a
nextCWM (DartM v p) =
    let d = degree v
    in DartM v ((p - 1) `mod` d)
