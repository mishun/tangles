module Graphics.HP.Transform
	(
	  Transform
	, identity
	, scaled
	, xscaled
	, yscaled
	, zscaled
	, shifted
	, rotated
	, slanted
	, xPart
	, yPart
	, xxPart
	, xyPart
	, yxPart
	, yyPart
	, composite
	, transform
	, inverse
	, transformPoint
	) where


type ProjVector = (Double, Double, Double)
type ProjMatrix = (ProjVector, ProjVector, ProjVector)


dot :: ProjVector -> ProjVector -> Double
dot (ax, ay, az) (bx, by, bz) = (ax * bx) + (ay * by) + (az * bz)


applyMatrix :: ProjMatrix -> (Double, Double, Double) -> (Double, Double, Double)
applyMatrix (rowX, rowY, rowZ) v = (dot rowX v, dot rowY v, dot rowZ v)


compositeMatrix :: ProjMatrix -> ProjMatrix -> ProjMatrix
compositeMatrix (rowX, rowY, rowZ) ((xx, xy, xz), (yx, yy, yz), (zx, zy, zz)) = (rx, ry, rz)
	where
		colX = (xx, yx, zx)
		colY = (xy, yy, zy)
		colZ = (xz, yz, zz)

		rx = (dot rowX colX, dot rowX colY, dot rowX colZ)
		ry = (dot rowY colX, dot rowY colY, dot rowY colZ)
		rz = (dot rowZ colX, dot rowZ colY, dot rowZ colZ)


inverseMatrix :: ProjMatrix -> ProjMatrix
inverseMatrix ((xx, xy, xz), (yx, yy, yz), (zx, zy, zz)) =
	(
		((yy * zz - yz * zy) / det, (xz * zy - xy * zz) / det, (xy * yz - xz * yy) / det),
		((yz * zx - yx * zz) / det, (xx * zz - xz * zx) / det, (xz * yx - xx * yz) / det),
		((yx * zy - yy * zx) / det, (xy * zx - xx * zy) / det, (xx * yy - xy * yx) / det)
	)

	where
		det = xx * yy * zz + xy * yz * zx + xz * yx * zy - xx * yz * zy - xy * yx * zz - xz * yy * zx


data Transform = Transform ProjMatrix deriving (Eq, Show)


identity :: Transform
identity = Transform ((1, 0, 0), (0, 1, 0), (0, 0, 1))


scaled :: Double -> Transform
scaled f = Transform ((f, 0, 0), (0, f, 0), (0, 0, 1))


xscaled :: Double -> Transform
xscaled f = Transform ((f, 0, 0), (0, 1, 0), (0, 0, 1))


yscaled :: Double -> Transform
yscaled f = Transform ((1, 0, 0), (0, f, 0), (0, 0, 1))


zscaled :: (Double, Double) -> Transform
zscaled (a, b) = Transform ((a, -b, 0), (b, a, 0), (0, 0, 1))


shifted :: (Double, Double) -> Transform
shifted (x, y) = Transform ((1, 0, x), (0, 1, y), (0, 0, 1))


rotated :: Double -> Transform
rotated angleDeg = Transform ((c, -s, 0), (s, c, 0), (0, 0, 1))
	where
		angle = pi * angleDeg / 180
		c = cos(angle)
		s = sin(angle)


slanted :: Double -> Transform
slanted a = Transform ((1, a, 0), (0, 1, 0), (0, 0, 1))


xPart :: Transform -> Double
xPart (Transform ((_, _, x), _, _)) = x


yPart :: Transform -> Double
yPart (Transform (_, (_, _, y), _)) = y


xxPart :: Transform -> Double
xxPart (Transform ((xx, _, _), _, _)) = xx


xyPart :: Transform -> Double
xyPart (Transform ((_, xy, _), _, _)) = xy


yxPart :: Transform -> Double
yxPart (Transform (_, (yx, _, _), _)) = yx


yyPart :: Transform -> Double
yyPart (Transform (_, (_, yy, _), _)) = yy


composite :: Transform -> Transform -> Transform
composite (Transform a) (Transform b) = Transform $ compositeMatrix a b


transform :: [Transform] -> Transform
transform list = foldr composite identity list


inverse :: Transform -> Transform
inverse (Transform t) = Transform $ inverseMatrix t


transformPoint :: Transform -> (Double, Double) -> (Double, Double)
transformPoint (Transform m) (x, y) = (rx / rz, ry / rz)
	where
		(rx, ry, rz) = applyMatrix m (x, y, 1)
