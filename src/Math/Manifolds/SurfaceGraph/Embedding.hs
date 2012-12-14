module Math.Manifolds.SurfaceGraph.Embedding
	( embeddingWithVertexRooting
	, embeddingWithFaceRooting
	) where

import qualified Data.Sequence as Seq
import qualified System.IO.Unsafe as IOUnsafe
import Data.Maybe
import Data.List
import Data.Array
import Data.Array.MArray
import Data.Array.Storable (withStorableArray)
import qualified Data.Array.IO as IOArray
import qualified Data.IORef as IORef
import Control.Monad
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CSize(..), CDouble(..))
import qualified Foreign.Marshal.Array as FMA
import Foreign.Marshal.Alloc (free)
import Math.Manifolds.SurfaceGraph
import Math.Manifolds.SurfaceGraph.Util


embeddingWithVertexRooting :: Int -> Vertex -> Array Dart [(Double, Double)]
embeddingWithVertexRooting n v = smoothEmbedding n (Left v)


embeddingWithFaceRooting :: Int -> Face -> Array Dart [(Double, Double)]
embeddingWithFaceRooting n f = smoothEmbedding n (Right f)


smoothEmbedding :: Int -> Either Vertex Face -> Array Dart [(Double, Double)]
smoothEmbedding subdivisionOrder root =
	relaxEmbedding root $!
		barycentricImproving
			(\ (Left v) -> quadraticEmbedding v)
			(max 1 subdivisionOrder)
			root


barycentricImproving :: (Either Vertex Face -> Array Dart [(Double, Double)]) -> Int -> Either Vertex Face -> Array Dart [(Double, Double)]
barycentricImproving f n root
	| n <= 0     = f root
	| otherwise  =
		let	g = case root of { Left vertex -> vertexOwnerGraph vertex ; Right face -> faceOwnerGraph face }
			(_, vv, vf, vd) = barycentricSubdivision' g
			be = barycentricImproving f (n - 1) $! Left $!
				case root of
					Left vertex -> fst $! fromJust $! find ((== vertex) . snd) vv
					Right face  -> fst $! fromJust $! find ((== face) . snd) vf

		in array (dartsRange g) $! concatMap (\ (v, (a, b)) ->
				let l = (reverse $ be ! nthDartIncidentToVertex v 0) ++ tail (be ! nthDartIncidentToVertex v 2)
				in [(a, l), (b, reverse l)]
			) vd


quadraticEmbedding :: Vertex -> Array Dart [(Double, Double)]
quadraticEmbedding root =
	let	g = vertexOwnerGraph root
		c = quadraticInitialization 0.99 root $!
			let k = vertexDegree root
			in map (\ !i -> let a = 2 * pi * fromIntegral i / fromIntegral k in (cos a, -sin a)) [0 .. k - 1]
	in listArray (dartsRange g) $! map (\ d -> [c ! d, c ! opposite d]) $! graphDarts g


foreign import ccall "_ZN4Math9Manifolds9Embedding22conjugateGradientSolveEjjPKjPKdS5_Pd"
	c_conjugateGradientSolve :: CSize -> CSize -> Ptr CSize -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> IO CDouble

quadraticInitialization :: Double -> Vertex -> [(Double, Double)] -> Array Dart (Double, Double)
quadraticInitialization seed s brd
	| vertexDegree s /= length brd  = error "quadraticInitialization: wrong number of elements in border"
	| otherwise                     = IOUnsafe.unsafePerformIO $ do
		let g = vertexOwnerGraph s
		let vi = listArray (verticesRange g) $! scanl (\ !x !v -> if v == s then x else x + 1) (0 :: Int) $! graphVertices g

		let n = numberOfVertices g - 1
		x <- newArray (0, n - 1) 0
		y <- newArray (0, n - 1) 0
		do
			dist <- do
				d <- newArray (verticesRange g) (-1) :: IO (IOArray.IOArray Vertex Int)
				writeArray d s 0
				q <- IORef.newIORef (Seq.singleton s)

				let isEmpty = IORef.readIORef q >>= return . Seq.null
				let enqueue u = IORef.modifyIORef q (Seq.|> u)
				let dequeue = do
					qc <- IORef.readIORef q
					let (u Seq.:< rest) = Seq.viewl qc
					IORef.writeIORef q rest
					return $! u

				let loop = do
					empty <- isEmpty
					when (not empty) $ do
						u <- dequeue
						du <- readArray d u
						forM_ (adjacentVertices u) $ \ v -> do
							dv <- readArray d v
							when (dv < 0) $ do
								writeArray d v (du + 1)
								enqueue v
						loop

				loop
				return $! d

			let dartWeight d = do
				l <- readArray dist (beginVertex d)
				r <- readArray dist (endVertex d)
				return $! (realToFrac seed) ** realToFrac (min l r)

			defs <- do
				a <- forM (filter (/= s) $ graphVertices g) $ \ v -> do
					w <- foldM (\ !w !d -> dartWeight d >>= return . (w +)) (0 :: Double) $! dartsIncidentToVertex v
					let i = fromIntegral (vi ! v)
					return $! (i, i, realToFrac w)

				b <- forM (filter (\ !d -> beginVertex d /= s && endVertex d /= s) $ graphDarts g) $ \ !d -> do
					w <- dartWeight d
					return $! (fromIntegral (vi ! beginVertex d), fromIntegral (vi ! endVertex d), -w)

				return $! a ++ b

			let m = length defs

			coords <- newArray_ (0, 2 * m - 1)
			a <- newArray_ (0, m - 1)
			forM_ (zip [0 ..] defs) $ \ (!i, (!cx, !cy, !d)) -> do
				writeArray a i d
				writeArray coords (2 * i) cx
				writeArray coords (2 * i + 1) cy

			bx <- newArray (0, n - 1) 0
			by <- newArray (0, n - 1) 0
			forM_ (zip brd (dartsIncidentToVertex s)) $ \ ((cx, cy), d) ->
				when (s /= endVertex d) $ do
					let i = vi ! (endVertex d)
					w <- dartWeight d
					readArray bx i >>= writeArray bx i . (+ realToFrac (cx * w))
					readArray by i >>= writeArray by i . (+ realToFrac (cy * w))

			withStorableArray coords $ \ pc -> withStorableArray a $ \ pa -> do
				withStorableArray bx $ \ pb -> withStorableArray x $ \ px ->
					void $! c_conjugateGradientSolve (fromIntegral n) (fromIntegral m) pc pa pb px
				withStorableArray by $ \ pb -> withStorableArray y $ \ py ->
					void $! c_conjugateGradientSolve (fromIntegral n) (fromIntegral m) pc pa pb py

		let border = listArray (0, length brd - 1) brd
		result <- forM (graphDarts g) $ \ !d -> do
				let (v, p) = begin d
				if v == s
					then return $! border ! p
					else do
						cx <- readArray x (vi ! v)
						cy <- readArray y (vi ! v)
						return $! (realToFrac cx, realToFrac cy)

		return $! listArray (dartsRange g) result


foreign import ccall "_ZN4Math9Manifolds9Embedding14relaxEmbeddingERKNS1_16InteractionConstEjjPNS_7Numeric7Vector2EjPKjPKS9_jS9_SB_"
	c_relaxEmbedding :: Ptr CDouble
		-> CSize -> CSize -> Ptr CDouble
		-> CSize -> Ptr CSize -> Ptr (Ptr CSize)
		-> CSize -> Ptr CSize -> Ptr (Ptr CSize)
		-> IO ()

relaxEmbedding :: Either Vertex Face -> Array Dart [(Double, Double)] -> Array Dart [(Double, Double)]
relaxEmbedding root initial
	| not $ all (even . vertexDegree) $ graphVertices g        = error "relaxEmbedding: all vertices must have even degree"
	| not $ all ((> 1) . length . (initial !)) $ graphDarts g  = error "relaxEmbedding: there must be at least 2 point at every dart"
	| otherwise                                                = IOUnsafe.unsafePerformIO $ do
		let numberOfFrozenPoints = case root of { Left v -> vertexDegree v ; Right _ -> 0 }
		let totalNumberOfPoints =
			(sum $ map ((\ x -> x - 2) . length . (initial !) . fst) $ graphEdges g)
				+ (numberOfVertices g) + (max 0 $ numberOfFrozenPoints - 1)
		let numberOfMovablePoints = totalNumberOfPoints - numberOfFrozenPoints

		dartBeginIndex <- do
			dartBeginIndex <- newArray_ (dartsRange g) :: IO (IOArray.IOArray Dart a)
			forM_ (graphDarts g) $ \ !d ->
				writeArray dartBeginIndex d $
					let v = beginVertex d
					in case root of
						Left start ->
							case compare v start of
								EQ -> totalNumberOfPoints - vertexDegree start + (snd $ begin d)
								LT -> vertexIndex v
								GT -> vertexIndex v - 1
						Right _    -> vertexIndex v

			freeze dartBeginIndex :: IO (Array Dart Int)

		(dartIndices, threads) <- do
			dartIndices <- newArray_ (dartsRange g) :: IO (IOArray.IOArray Dart [Int])
			freeIndex <- IORef.newIORef (case root of { Left _ -> numberOfVertices g - 1 ; Right _ -> numberOfVertices g })

			let allocate a = do
				let b = opposite a
				let len = length (initial ! a) - 2
				gotIndex <- IORef.readIORef freeIndex
				IORef.writeIORef freeIndex (gotIndex + len)
				let list = [dartBeginIndex ! a] ++ [gotIndex .. gotIndex + len - 1] ++ [dartBeginIndex ! b]
				writeArray dartIndices a list
				writeArray dartIndices b $! reverse list
				return $! list

			visited <- newArray (dartsRange g) False :: IO (IOArray.IOArray Dart Bool)

			let walk thread first a = do
				let b = opposite a
				writeArray visited a True
				writeArray visited b True
				nextThread <- ((++ thread) . reverse . tail) `fmap` (allocate a)
				let v = beginVertex b
				let cont = nthDartIncidentToVertex v $! (snd $ begin b) + (vertexDegree v `div` 2)
				if Left v == root || cont == first
					then return $! nextThread
					else walk nextThread first cont

			threads <- IORef.newIORef []
			let tryWalk d = do
				v <- readArray visited d
				unless v $ do
					thread <- walk [dartBeginIndex ! d] d d
					IORef.modifyIORef threads (thread :)

			case root of
				Left v -> forM_ (dartsIncidentToVertex v) tryWalk
				_      -> return ()
			forM_ (graphDarts g) tryWalk

			dartIndices' <- freeze dartIndices :: IO (Array Dart [Int])
			threads' <- IORef.readIORef threads
			return $! (dartIndices' , threads' )

		let numberOfThreads = length threads

		let interaction =
			[ 2    -- border
			, 0.5  -- electric
			, 15   -- bend
			, 10   -- elastic
			, 1.5  -- cross
			]

		FMA.withArray interaction $ \ interactionPtr -> do
			coords <- newArray (0, 2 * totalNumberOfPoints - 1) 0.0
			forM_ (graphEdges g) $ \ (d, _) ->
				forM_ (zip (initial ! d) (dartIndices ! d)) $ \ ((x, y), i) -> do
					writeArray coords (2 * i) (realToFrac x)
					writeArray coords (2 * i + 1) (realToFrac y)

			withStorableArray coords $ \ xPtr ->
				FMA.withArray (map (fromIntegral . length) threads) $ \ lensPtr -> do
					threadPtrs <- newArray_ (0, numberOfThreads - 1)
					forM_ (zip threads [0 ..]) $ \ (thread, i) ->
						(FMA.newArray $! map fromIntegral thread) >>= writeArray threadPtrs i

					withStorableArray threadPtrs $ \ threadsPtrsPtr -> do
						let aliveVertices = filter ((/= root) . Left) $! graphVertices g
						FMA.withArray (map (fromIntegral . vertexDegree) aliveVertices) $ \ vertexDegreePtr -> do
							adjPtrs <- newArray_ (0, length aliveVertices - 1)
							forM_ (zip aliveVertices [0 ..]) $ \ (v, i) ->
								(FMA.newArray $! map (fromIntegral . (!! 1) . (dartIndices !)) (dartsIncidentToVertex v))
									>>= writeArray adjPtrs i

							withStorableArray adjPtrs $ \ adjPtrsPtr ->
								c_relaxEmbedding interactionPtr
									(fromIntegral numberOfMovablePoints) (fromIntegral numberOfFrozenPoints) xPtr
									(fromIntegral numberOfThreads) lensPtr threadsPtrsPtr
									(fromIntegral $ length aliveVertices) vertexDegreePtr adjPtrsPtr

							getElems adjPtrs >>= mapM_ free

					getElems threadPtrs >>= mapM_ free

			fmap (listArray (dartsRange g)) $ forM (graphDarts g) $ \ d -> forM (dartIndices ! d) $ \ i -> do
				x <- readArray coords (2 * i)
				y <- readArray coords (2 * i + 1)
				return $! (realToFrac x, realToFrac y)
	where
		g = case root of
			Left v  -> vertexOwnerGraph v
			Right f -> faceOwnerGraph f
