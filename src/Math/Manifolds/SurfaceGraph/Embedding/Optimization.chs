module Math.Manifolds.SurfaceGraph.Embedding.Optimization
	( InteractionConst(..)
	, relaxEmbedding'
	, conjugateGradientSolve'
	) where

import Data.Array.MArray (newListArray, getElems)
import Data.Array.Storable (StorableArray, withStorableArray)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (void, liftM)
import Foreign.Marshal.Utils (with, withMany)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable(..))
import Foreign.C.Types
import Foreign.Marshal.Alloc (free)
import Foreign.Marshal.Array (newArray, withArray)

#include <Math/Manifolds/SurfaceGraph/Embedding/Optimization.h>


data InteractionConst = InteractionConst
	{ interactionBorder   :: Double
	, interactionElectric :: Double
	, interactionBend     :: Double
	, interactionElastic  :: Double
	, interactionCross    :: Double
	}

{# pointer *InteractionConst as InteractionConstPtr -> InteractionConst #}

instance Storable InteractionConst where
	sizeOf _ = {# sizeof InteractionConst #}

	alignment _ = {# alignof InteractionConst #}

	peek p = InteractionConst
		<$> liftM realToFrac ({# get InteractionConst.border   #} p)
		<*> liftM realToFrac ({# get InteractionConst.electric #} p)
		<*> liftM realToFrac ({# get InteractionConst.bend     #} p)
		<*> liftM realToFrac ({# get InteractionConst.elastic  #} p)
		<*> liftM realToFrac ({# get InteractionConst.cross    #} p)

	poke p x = do
		{# set InteractionConst.border   #} p $ realToFrac $ interactionBorder x
		{# set InteractionConst.electric #} p $ realToFrac $ interactionElectric x
		{# set InteractionConst.bend     #} p $ realToFrac $ interactionBend x
		{# set InteractionConst.elastic  #} p $ realToFrac $ interactionElastic x
		{# set InteractionConst.cross    #} p $ realToFrac $ interactionCross x


foreign import ccall "_ZN4Math9Manifolds9Embedding12Optimization14relaxEmbeddingERKNS2_16InteractionConstEjjPNS_7Numeric7Vector2EjPKjPKSA_jSA_SC_"
	c_relaxEmbedding :: InteractionConstPtr
		-> CSize -> CSize -> Ptr CDouble
		-> CSize -> Ptr CSize -> Ptr (Ptr CSize)
		-> CSize -> Ptr CSize -> Ptr (Ptr CSize)
		-> IO ()


relaxEmbedding' :: InteractionConst -> Int -> Int -> StorableArray Int CDouble -> [[Int]] -> [[Int]] -> IO ()
relaxEmbedding' interaction numberOfMovablePoints numberOfFrozenPoints coords threads aliveVertices =
	with interaction $ \ interactionPtr ->
		withStorableArray coords $ \ xPtr -> do
			let numberOfThreads = length threads
			threadPtrs <- newListArray (0, numberOfThreads - 1) =<< mapM (newArray . map fromIntegral) threads

			let numberOfAliveVertices = length aliveVertices
			adjPtrs <- newListArray (0, numberOfAliveVertices - 1) =<< mapM (newArray . map fromIntegral) aliveVertices

			withStorableArray threadPtrs $ \ threadsPtrsPtr ->
				withArray (map (fromIntegral . length) threads) $ \ lensPtr ->
					withArray (map (fromIntegral . length) aliveVertices) $ \ vertexDegreePtr ->
						withStorableArray adjPtrs $ \ adjPtrsPtr ->
							c_relaxEmbedding interactionPtr
								(fromIntegral numberOfMovablePoints) (fromIntegral numberOfFrozenPoints) xPtr
								(fromIntegral numberOfThreads) lensPtr threadsPtrsPtr
								(fromIntegral numberOfAliveVertices) vertexDegreePtr adjPtrsPtr

			getElems adjPtrs >>= mapM_ free
			getElems threadPtrs >>= mapM_ free


foreign import ccall "_ZN4Math9Manifolds9Embedding12Optimization22conjugateGradientSolveEjjPKjPKdS6_Pd"
	c_conjugateGradientSolve :: CSize -> CSize -> Ptr CSize -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CDouble


conjugateGradientSolve' :: Int -> Int -> StorableArray Int CSize -> StorableArray Int CDouble -> StorableArray Int CDouble -> StorableArray Int CDouble -> IO ()
conjugateGradientSolve' n m coords a b x =
	withStorableArray coords $ \ pc ->
		withMany withStorableArray [a, b, x] $ \ [pa, pb, px] ->
			void $ c_conjugateGradientSolve (fromIntegral n) (fromIntegral m) pc pa pb px
