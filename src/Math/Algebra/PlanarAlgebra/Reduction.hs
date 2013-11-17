{-# LANGUAGE RankNTypes #-}
module Math.Algebra.PlanarAlgebra.Reduction
    ( reduceWithStrategy
    , VertexM
    , DartM
    , nextCCWM
    , nextCWM
    , oppositeM
    ) where

import Data.Function (fix)
import Data.Array.Base (unsafeRead, unsafeAt)
import Data.Array.Unboxed (UArray)
import Data.Array.ST (STArray)
import Data.STRef (STRef, newSTRef)
import Control.Monad.Reader (ReaderT, runReaderT, ask, lift)
import Control.Monad.ST (ST, runST)
import Math.Algebra.PlanarAlgebra.Definition


reduceWithStrategy :: (PlanarAlgebra a, PlanarState t) => a x -> (Vertex a x -> t) -> (forall s. Strategy s t) -> t
reduceWithStrategy _ _ strategy =
    runST $ do
        context <- do
            fi' <- newSTRef 0
            return Context { oa = undefined, fi = fi' }

        flip runReaderT context $
            fix $ \ continue -> do
                action <- strategy
                case action of
                    Return r   -> return r
                    Contract _ -> continue


data Context s a =
    Context
        { oa :: {-# UNPACK #-} !(STArray s Int (DartM a))
        , fi :: {-# UNPACK #-} !(STRef s Int)
        }


type PlanarM s a = ReaderT (Context s a) (ST s)


data StrategyResult a = Return a | Contract (DartM a)

type Strategy s a = PlanarM s a (StrategyResult a)


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


nextCCWM :: DartM a -> DartM a
nextCCWM (DartM v p) =
    let d = degree v
    in DartM v ((p + 1) `mod` d)


nextCWM :: DartM a -> DartM a
nextCWM (DartM v p) =
    let d = degree v
    in DartM v ((p - 1) `mod` d)


oppositeM :: DartM a -> PlanarM s a (DartM a)
oppositeM (DartM v p) =
    ask >>= \ context -> lift $
        unsafeRead (oa context) (incidence v `unsafeAt` p)
