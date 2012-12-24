module Math.KnotTh.Link.GaussCodesList
	( gaussCodes
	) where


gaussCodes :: [((Int, Int), [[[Int]]])]
gaussCodes =
	[ ((2, 2),
		[ [[1, -2], [2, -1]]
		])
	
	, ((3, 1),
		[ [[-1, 3, -2, 1, -3, 2]]
		])

	, ((4, 1),
		[ [[1, -4, 3, -1, 2, -3, 4, -2]]
		])

	, ((4, 2),
		[ [[1, -3, 2, -4], [3, -1, 4, -2]]
		])

	, ((5, 1),
		[ [[-1, 4, -2, 5, -3, 1, -4, 2, -5, 3]]
		, [[-1, 5, -2, 1, -3, 4, -5, 2, -4, 3]]
		])

	, ((5, 2),
		[ [[1, -4, 5, -3], [3, -1, 2, -5, 4, -2]]
		])

	, ((6, 1),
		[ [[-1, 4, -3, 1, -5, 6, -2, 3, -4, 2, -6, 5]]
		, [[-1, 4, -3, 1, -2, 6, -5, 3, -4, 2, -6, 5]]
		, [[1, -6, 2, -1, 4, -5, 6, -2, 3, -4, 5, -3]]
		])

	, ((6, 2),
		[ [[1, -5, 2, -6], [5, -1, 3, -4, 6, -2, 4, -3]]
		, [[1, -5, 3, -4, 2, -6], [5, -1, 6, -3, 4, -2]]
		, [[1, -2, 3, -6, 4, -5], [5, -1, 2, -3, 6, -4]]
		])

	, ((6, 3),
		[ [[1, -6, 5, -3], [4, -1, 2, -5], [6, -4, 3, -2]]
		, [[1, -5, 2, -6], [5, -1, 3, -4], [6, -2, 4, -3]]
		, [[1, 6, -5, -3], [-4, -1, 2, 5], [-6, 4, 3, -2]]
		])

	, ((7, 1),
		[ [[-1, 5, -2, 6, -3, 7, -4, 1, -5, 2, -6, 3, -7, 4]]
		, [[-1, 7, -2, 1, -3, 6, -4, 5, -7, 2, -5, 4, -6, 3]]
		, [[1, -6, 2, -7, 5, -1, 3, -4, 6, -2, 7, -5, 4, -3]]
		, [[1, -5, 6, -7, 2, -1, 3, -4, 7, -6, 5, -2, 4, -3]]
		, [[-1, 7, -2, 1, -3, 5, -4, 6, -7, 2, -6, 3, -5, 4]]
		, [[-1, 4, -5, 3, -6, 5, -4, 2, -7, 6, -3, 1, -2, 7]]
		, [[-1, 4, -3, 1, -2, 7, -6, 3, -4, 2, -5, 6, -7, 5]]
		])

	, ((7, 2),
		[ [[1, -7, 5, -3], [4, -1, 2, -5, 6, -4, 7, -2, 3, -6]]
		, [[1, -6, 2, -7], [6, -1, 4, -5, 7, -2, 3, -4, 5, -3]]
		, [[1, -6, 2, -7], [6, -1, 3, -5, 4, -2, 7, -3, 5, -4]]
		, [[1, -6, 2, -7], [6, -1, 3, -5, 4, -2, 7, -4, 5, -3]]
		, [[1, -6, 2, -5, 3, -7], [6, -1, 7, -2, 4, -3, 5, -4]]
		, [[1, -5, 2, -6, 4, -7], [5, -1, 3, -2, 6, -4, 7, -3]]
		, [[1, 7, -5, -3], [-4, -1, 2, 5, -6, 4, -7, -2, 3, 6]]
		, [[1, -7, -5, 3], [-4, -1, 2, 5, -6, 4, 7, -2, -3, 6]]
		])

	, ((7, 3),
		[ [[1, -6, 2, -7], [6, -1, 5, -4], [7, -2, 3, -5, 4, -3]]
		])

	, ((8, 1),
		[ [[-1, 4, -3, 1, -5, 8, -6, 7, -2, 3, -4, 2, -7, 6, -8, 5]]
		, [[-1, 4, -3, 1, -2, 7, -5, 8, -6, 3, -4, 2, -7, 5, -8, 6]]
		, [[1, -6, 4, -5, 3, -1, 7, -8, 2, -3, 5, -4, 6, -2, 8, -7]]
		, [[1, -4, 3, -7, 5, -1, 6, -8, 2, -3, 7, -5, 4, -2, 8, -6]]
		, [[1, -3, 2, -6, 5, -1, 3, -2, 4, -8, 7, -5, 6, -4, 8, -7]]
		, [[-1, 4, -3, 1, -5, 7, -6, 8, -2, 3, -4, 2, -8, 5, -7, 6]]
		, [[-1, 8, -2, 1, -4, 6, -5, 7, -8, 2, -3, 4, -6, 5, -7, 3]]
		, [[-1, 8, -2, 1, -4, 5, -8, 2, -6, 7, -3, 4, -5, 3, -7, 6]]
		, [[1, -4, 3, -6, 5, -1, 2, -8, 7, -3, 6, -5, 4, -2, 8, -7]]
		, [[-1, 8, -2, 1, -4, 5, -8, 2, -3, 7, -6, 4, -5, 3, -7, 6]]
		, [[-1, 4, -3, 1, -2, 6, -7, 8, -5, 3, -4, 2, -8, 7, -6, 5]]
		, [[1, -4, 3, -1, 5, -8, 2, -3, 4, -2, 6, -7, 8, -5, 7, -6]]
		, [[-1, 8, -2, 1, -4, 5, -6, 7, -8, 2, -3, 4, -7, 6, -5, 3]]
		, [[-1, 4, -3, 1, -2, 8, -5, 3, -4, 2, -6, 7, -8, 5, -7, 6]]
		, [[-1, 8, -2, 1, -3, 7, -8, 2, -5, 6, -7, 3, -4, 5, -6, 4]]
		, [[1, -8, 5, -6, 2, -1, 4, -5, 6, -7, 3, -4, 8, -2, 7, -3]]
		, [[1, -4, 3, -6, 5, -1, 2, -3, 6, -8, 7, -5, 4, -2, 8, -7]]
		, [[1, -4, 2, -5, 6, -1, 7, -2, 8, -6, 3, -7, 4, -8, 5, -3]]
		, [[1, -8, 2, -1, -4, 5, 8, -2, -3, 7, -6, 4, -5, 3, -7, 6]]
		, [[1, -8, 2, -1, -3, 7, 8, -2, -5, 6, -7, 3, -4, 5, -6, 4]]
		, [[-1, 8, -2, 1, 3, -7, -8, 2, -5, 6, 7, -3, -4, 5, -6, 4]]
		])

	, ((9, 1),
		[ [[-1, 6, -2, 7, -3, 8, -4, 9, -5, 1, -6, 2, -7, 3, -8, 4, -9, 5]]
		, [[-1, 9, -2, 1, -3, 8, -4, 7, -5, 6, -9, 2, -6, 5, -7, 4, -8, 3]]
		, [[1, -7, 2, -8, 5, -9, 6, -1, 3, -4, 7, -2, 8, -5, 9, -6, 4, -3]]
		, [[-1, 8, -2, 9, -7, 1, -3, 6, -4, 5, -8, 2, -9, 7, -5, 4, -6, 3]]
		, [[1, -7, 8, -9, 2, -1, 3, -6, 4, -5, 9, -8, 7, -2, 5, -4, 6, -3]]
		, [[-1, 9, -2, 1, -3, 6, -4, 7, -5, 8, -9, 2, -8, 3, -6, 4, -7, 5]]
		, [[-1, 9, -2, 1, -3, 5, -4, 8, -6, 7, -9, 2, -7, 6, -8, 3, -5, 4]]
		, [[-1, 9, -2, 1, -3, 8, -9, 2, -4, 7, -5, 6, -8, 3, -6, 5, -7, 4]]
		, [[-1, 8, -2, 9, -7, 1, -3, 5, -4, 6, -8, 2, -9, 7, -6, 3, -5, 4]]
		, [[1, -6, 2, -7, 8, -9, 5, -1, 3, -4, 6, -2, 9, -8, 7, -5, 4, -3]]
		, [[-1, 4, -3, 1, -6, 8, -7, 9, -2, 3, -4, 2, -5, 6, -8, 7, -9, 5]]
		, [[-1, 4, -5, 3, -8, 6, -7, 5, -4, 2, -9, 7, -6, 8, -3, 1, -2, 9]]
		, [[1, -7, 8, -9, 2, -1, 3, -5, 4, -6, 9, -8, 7, -2, 6, -3, 5, -4]]
		, [[-1, 4, -3, 1, -2, 9, -7, 8, -6, 3, -4, 2, -5, 6, -8, 7, -9, 5]]
		, [[-1, 4, -3, 1, -6, 7, -2, 3, -4, 2, -8, 9, -5, 6, -7, 5, -9, 8]]
		, [[1, -9, 2, -1, 3, -5, 4, -8, 7, -6, 9, -2, 8, -7, 6, -3, 5, -4]]
		, [[-1, 4, -3, 1, -2, 6, -5, 9, -8, 3, -4, 2, -6, 5, -7, 8, -9, 7]]
		, [[-1, 9, -2, 1, -3, 5, -6, 7, -4, 8, -9, 2, -8, 3, -7, 6, -5, 4]]
		, [[-1, 4, -3, 1, -2, 7, -6, 3, -4, 2, -8, 9, -5, 6, -7, 5, -9, 8]]
		, [[-1, 5, -7, 4, -6, 3, -8, 7, -5, 2, -9, 8, -4, 6, -3, 1, -2, 9]]
		, [[-1, 4, -3, 1, -6, 7, -8, 9, -2, 3, -4, 2, -5, 6, -9, 8, -7, 5]]
		, [[1, -4, 3, -1, 2, -7, 6, -3, 4, -2, 5, -9, 8, -6, 7, -5, 9, -8]]
		, [[-1, 9, -2, 1, -3, 7, -4, 8, -9, 2, -8, 3, -5, 6, -7, 4, -6, 5]]
		, [[-1, 9, -2, 1, -3, 8, -9, 2, -4, 6, -5, 7, -8, 3, -7, 4, -6, 5]]
		, [[-1, 9, -2, 1, -3, 8, -9, 2, -4, 7, -8, 3, -5, 6, -7, 4, -6, 5]]
		, [[-1, 4, -3, 1, -2, 7, -6, 9, -8, 3, -4, 2, -5, 6, -9, 8, -7, 5]]
		, [[-1, 9, -2, 1, -4, 7, -6, 8, -9, 2, -3, 4, -5, 6, -7, 5, -8, 3]]
		, [[-1, 9, -2, 1, -4, 5, -9, 2, -7, 8, -3, 4, -5, 3, -6, 7, -8, 6]]
		, [[1, -4, 3, -7, 5, -1, 6, -9, 7, -3, 2, -8, 9, -5, 4, -2, 8, -6]]
		, [[1, -4, 3, -1, 2, -9, 5, -3, 4, -2, 7, -8, 9, -5, 6, -7, 8, -6]]
		, [[-1, 9, -2, 1, -4, 5, -6, 8, -9, 2, -3, 4, -7, 6, -8, 7, -5, 3]]
		, [[-1, 4, -3, 1, -7, 9, -5, 3, -4, 6, -8, 7, -2, 5, -6, 8, -9, 2]]
		, [[1, -4, 3, -1, 6, -7, 2, -3, 4, -9, 8, -2, 5, -6, 9, -8, 7, -5]]
		, [[1, -4, 3, -7, 6, -1, 2, -3, 5, -6, 8, -9, 7, -5, 4, -2, 9, -8]]
		, [[-1, 7, -8, 9, -3, 5, -2, 1, -4, 6, -9, 8, -7, 2, -5, 3, -6, 4]]
		, [[-1, 4, -3, 1, -6, 7, -2, 3, -4, 2, -5, 9, -8, 6, -7, 5, -9, 8]]
		, [[-1, 4, -3, 1, -5, 6, -2, 9, -8, 3, -4, 2, -6, 5, -7, 8, -9, 7]]
		, [[-1, 9, -5, 6, -2, 1, -3, 4, -6, 5, -8, 7, -9, 2, -4, 8, -7, 3]]
		, [[-1, 8, -2, 7, -6, 1, -3, 9, -5, 2, -8, 4, -9, 6, -7, 5, -4, 3]]
		, [[-1, 4, -6, 7, -3, 1, -2, 8, -7, 5, -4, 2, -9, 3, -5, 6, -8, 9]]
		, [[1, -5, 4, -6, 3, -1, 2, -7, 8, -4, 5, -2, 9, -3, 6, -8, 7, -9]]
		, [[-1, 4, -3, 1, -2, -7, 6, 3, -4, 2, 5, -9, 8, -6, 7, -5, 9, -8]]
		, [[1, 6, -8, 7, -6, -5, 9, 8, -7, -2, 4, -3, 5, -9, 2, -1, 3, -4]]
		, [[-1, 4, -3, 1, -2, -9, 5, 3, -4, 2, 7, -8, 9, -5, 6, -7, 8, -6]]
		, [[1, -4, 3, -1, 2, 9, -5, -3, 4, -2, 7, -8, -9, 5, 6, -7, 8, -6]]
		, [[1, -4, 3, -1, -5, 6, -2, 9, -8, -3, 4, 2, -6, 5, -7, 8, -9, 7]]
		, [[1, -4, 3, -7, 6, -1, 2, -3, 5, -6, -8, 9, 7, -5, 4, -2, -9, 8]]
		, [[-1, 4, -3, 1, 5, -6, 2, 9, -8, 3, -4, -2, 6, -5, -7, 8, -9, 7]]
		, [[1, 5, -4, 6, -3, -1, 2, 7, -8, 4, -5, -2, 9, 3, -6, 8, -7, -9]]
		])

	, ((10, 1),
		[ [[-1, 4, -3, 1, -5, 10, -6, 9, -7, 8, -2, 3, -4, 2, -8, 7, -9, 6, -10, 5]]
		, [[-1, 4, -3, 1, -2, 8, -5, 9, -6, 10, -7, 3, -4, 2, -8, 5, -9, 6, -10, 7]]
		, [[-1, 6, -4, 5, -3, 1, -7, 10, -8, 9, -2, 3, -5, 4, -6, 2, -9, 8, -10, 7]]
		, [[-1, 4, -3, 8, -5, 1, -6, 10, -7, 9, -2, 3, -8, 5, -4, 2, -9, 7, -10, 6]]
		, [[-1, 10, -2, 1, -4, 7, -5, 8, -6, 9, -10, 2, -3, 4, -7, 5, -8, 6, -9, 3]]
		, [[-1, 4, -3, 1, -5, 8, -6, 9, -7, 10, -2, 3, -4, 2, -10, 5, -8, 6, -9, 7]]
		, [[-1, 4, -3, 1, -2, 6, -7, 10, -8, 9, -5, 3, -4, 2, -9, 8, -10, 7, -6, 5]]
		, [[-1, 6, -4, 5, -3, 1, -2, 9, -7, 10, -8, 3, -5, 4, -6, 2, -9, 7, -10, 8]]
		, [[1, -4, 3, -6, 5, -1, 2, -9, 7, -10, 8, -3, 6, -5, 4, -2, 9, -7, 10, -8]]
		, [[-1, 10, -2, 1, -4, 5, -6, 9, -7, 8, -10, 2, -3, 4, -8, 7, -9, 6, -5, 3]]
		, [[-1, 6, -4, 5, -3, 1, -7, 9, -8, 10, -2, 3, -5, 4, -6, 2, -10, 7, -9, 8]]
		, [[-1, 10, -2, 1, -4, 6, -5, 7, -10, 2, -8, 9, -3, 4, -6, 5, -7, 3, -9, 8]]
		, [[-1, 4, -3, 1, -5, 10, -8, 9, -2, 3, -4, 2, -6, 7, -9, 8, -10, 5, -7, 6]]
		, [[-1, 4, -3, 1, -2, 9, -5, 10, -8, 3, -4, 2, -6, 7, -9, 5, -10, 8, -7, 6]]
		, [[-1, 10, -2, 1, -6, 8, -7, 9, -3, 4, -10, 2, -4, 3, -5, 6, -8, 7, -9, 5]]
		, [[1, -6, 4, -5, 3, -1, 2, -8, 9, -10, 7, -3, 5, -4, 6, -2, 10, -9, 8, -7]]
		, [[1, -9, 2, -10, 8, -1, 4, -6, 5, -7, 9, -2, 10, -8, 3, -4, 6, -5, 7, -3]]
		, [[-1, 10, -2, 1, -3, 8, -6, 7, -5, 9, -10, 2, -9, 3, -4, 5, -7, 6, -8, 4]]
		, [[-1, 9, -2, 10, -8, 1, -4, 5, -6, 7, -9, 2, -10, 8, -3, 4, -7, 6, -5, 3]]
		, [[-1, 4, -3, 1, -5, 7, -6, 10, -8, 9, -2, 3, -4, 2, -9, 8, -10, 5, -7, 6]]
		, [[-1, 4, -3, 1, -2, 7, -5, 8, -9, 10, -6, 3, -4, 2, -7, 5, -10, 9, -8, 6]]
		, [[1, -4, 3, -9, 5, -1, 6, -8, 7, -10, 2, -3, 9, -5, 4, -2, 10, -6, 8, -7]]
		, [[-1, 10, -2, 1, -4, 6, -5, 7, -8, 9, -10, 2, -3, 4, -6, 5, -9, 8, -7, 3]]
		, [[-1, 4, -3, 1, -5, 7, -8, 9, -6, 10, -2, 3, -4, 2, -10, 5, -9, 8, -7, 6]]
		, [[-1, 4, -3, 1, -2, 6, -8, 9, -7, 10, -5, 3, -4, 2, -10, 8, -9, 7, -6, 5]]
		, [[1, -4, 3, -6, 5, -1, 2, -8, 9, -10, 7, -3, 6, -5, 4, -2, 10, -9, 8, -7]]
		, [[1, -10, 2, -1, 4, -5, 7, -8, 6, -9, 10, -2, 3, -4, 9, -7, 8, -6, 5, -3]]
		, [[-1, 10, -2, 1, -4, 5, -6, 7, -10, 2, -8, 9, -3, 4, -7, 6, -5, 3, -9, 8]]
		, [[-1, 4, -3, 1, -5, 8, -6, 10, -2, 3, -4, 2, -7, 9, -10, 5, -8, 6, -9, 7]]
		, [[-1, 4, -3, 1, -2, 8, -9, 10, -5, 3, -4, 2, -6, 7, -10, 9, -8, 5, -7, 6]]
		, [[-1, 10, -2, 1, -6, 7, -8, 9, -3, 4, -10, 2, -4, 3, -5, 6, -9, 8, -7, 5]]
		, [[-1, 10, -2, 1, -3, 6, -5, 8, -7, 9, -10, 2, -9, 3, -4, 5, -8, 7, -6, 4]]
		, [[1, -8, 9, -10, 2, -1, 4, -5, 6, -7, 10, -9, 8, -2, 3, -4, 7, -6, 5, -3]]
		, [[-1, 10, -2, 1, -4, 5, -10, 2, -6, 9, -7, 8, -3, 4, -5, 3, -8, 7, -9, 6]]
		, [[-1, 4, -3, 1, -5, 10, -2, 3, -4, 2, -6, 9, -7, 8, -10, 5, -8, 7, -9, 6]]
		, [[-1, 4, -3, 1, -2, 10, -5, 3, -4, 2, -6, 9, -7, 8, -10, 5, -8, 7, -9, 6]]
		, [[1, -10, 2, -1, 6, -7, 3, -4, 10, -2, 4, -3, 8, -9, 5, -6, 7, -5, 9, -8]]
		, [[-1, 10, -2, 1, -3, 6, -5, 9, -10, 2, -9, 3, -7, 8, -4, 5, -6, 4, -8, 7]]
		, [[-1, 4, -3, 1, -2, 9, -5, 10, -6, 3, -4, 2, -9, 5, -7, 8, -10, 6, -8, 7]]
		, [[-1, 10, -2, 1, -4, 8, -5, 9, -10, 2, -3, 4, -8, 7, -6, 5, -9, 6, -7, 3]]
		, [[-1, 4, -3, 1, -2, 8, -9, 7, -5, 3, -4, 2, -7, 10, -6, 9, -8, 6, -10, 5]]
		, [[-1, 10, -2, 1, -4, 7, -8, 6, -10, 2, -3, 4, -6, 9, -5, 8, -7, 5, -9, 3]]
		, [[1, -10, 2, -1, 8, -9, 3, -6, 10, -2, 4, -5, 6, -3, 7, -8, 9, -7, 5, -4]]
		, [[-1, 4, -3, 1, -2, 10, -8, 9, -6, 3, -4, 2, -5, 6, -7, 8, -9, 7, -10, 5]]
		, [[1, -4, 3, -1, 2, -7, 6, -10, 9, -3, 4, -2, 5, -6, 8, -9, 10, -8, 7, -5]]
		, [[1, -3, 2, -6, 5, -1, 3, -2, 4, -9, 7, -10, 8, -5, 6, -4, 9, -7, 10, -8]]
		, [[-1, 10, -2, 1, -4, 5, -10, 2, -3, 8, -6, 9, -7, 4, -5, 3, -8, 6, -9, 7]]
		, [[1, -9, 2, -10, 3, -1, 9, -2, 5, -7, 6, -8, 10, -3, 4, -5, 7, -6, 8, -4]]
		, [[-1, 10, -2, 1, -3, 9, -10, 2, -5, 7, -6, 8, -9, 3, -4, 5, -7, 6, -8, 4]]
		, [[1, -3, 2, -6, 5, -1, 3, -2, 4, -8, 9, -10, 7, -5, 6, -4, 10, -9, 8, -7]]
		, [[-1, 10, -2, 1, -4, 5, -10, 2, -3, 7, -8, 9, -6, 4, -5, 3, -9, 8, -7, 6]]
		, [[1, -9, 2, -10, 3, -1, 9, -2, 5, -6, 7, -8, 10, -3, 4, -5, 8, -7, 6, -4]]
		, [[-1, 10, -2, 1, -3, 9, -10, 2, -5, 6, -7, 8, -9, 3, -4, 5, -8, 7, -6, 4]]
		, [[-1, 10, -2, 1, -6, 7, -3, 4, -10, 2, -4, 3, -5, 9, -8, 6, -7, 5, -9, 8]]
		, [[-1, 10, -2, 1, -3, 9, -10, 2, -5, 6, -9, 3, -7, 8, -4, 5, -6, 4, -8, 7]]
		, [[1, -10, 2, -1, 3, -6, 5, -9, 10, -2, 9, -3, 4, -8, 7, -5, 6, -4, 8, -7]]
		, [[-1, 10, -2, 1, -4, 5, -10, 2, -3, 9, -6, 4, -5, 3, -7, 8, -9, 6, -8, 7]]
		, [[-1, 4, -3, 1, -5, 10, -2, 3, -4, 2, -6, 9, -10, 5, -7, 8, -9, 6, -8, 7]]
		, [[1, -4, 3, -1, 2, -10, 5, -3, 4, -2, 6, -9, 10, -5, 7, -8, 9, -6, 8, -7]]
		, [[1, -4, 3, -1, 2, -7, 6, -3, 4, -2, 5, -10, 9, -6, 7, -5, 8, -9, 10, -8]]
		, [[1, -3, 2, -8, 6, -7, 5, -1, 3, -2, 4, -10, 9, -5, 7, -6, 8, -4, 10, -9]]
		, [[-1, 10, -2, 1, -4, 6, -5, 7, -10, 2, -3, 9, -8, 4, -6, 5, -7, 3, -9, 8]]
		, [[-1, 10, -2, 1, -3, 9, -7, 8, -10, 2, -5, 6, -8, 7, -9, 3, -4, 5, -6, 4]]
		, [[1, -3, 2, -6, 5, -8, 7, -1, 3, -2, 4, -10, 9, -5, 8, -7, 6, -4, 10, -9]]
		, [[-1, 10, -2, 1, -4, 5, -6, 7, -10, 2, -3, 9, -8, 4, -7, 6, -5, 3, -9, 8]]
		, [[-1, 10, -2, 1, -3, 5, -4, 9, -10, 2, -7, 8, -9, 3, -5, 4, -6, 7, -8, 6]]
		, [[-1, 4, -3, 1, -5, 6, -2, 10, -7, 3, -4, 2, -6, 5, -8, 9, -10, 7, -9, 8]]
		, [[1, -10, 2, -1, 4, -9, 8, -5, 6, -7, 10, -2, 3, -8, 9, -4, 7, -6, 5, -3]]
		, [[-1, 4, -3, 1, -6, 7, -2, 10, -9, 3, -4, 2, -5, 6, -7, 5, -8, 9, -10, 8]]
		, [[-1, 4, -3, 1, -5, 10, -2, 3, -4, 2, -6, 8, -7, 9, -10, 5, -9, 6, -8, 7]]
		, [[-1, 10, -2, 1, -4, 5, -10, 2, -6, 9, -3, 4, -5, 3, -7, 8, -9, 6, -8, 7]]
		, [[1, -4, 3, -1, 2, -10, 5, -3, 4, -2, 6, -8, 7, -9, 10, -5, 9, -6, 8, -7]]
		, [[1, -4, 3, -1, 2, -7, 6, -3, 4, -2, 9, -10, 5, -6, 7, -5, 8, -9, 10, -8]]
		, [[-1, 4, -3, 1, -2, 8, -9, 10, -6, 7, -5, 3, -4, 2, -10, 9, -8, 5, -7, 6]]
		, [[-1, 4, -3, 1, -2, 7, -6, 10, -9, 3, -4, 2, -5, 6, -7, 5, -8, 9, -10, 8]]
		, [[1, -4, 3, -1, 5, -7, 6, -8, 2, -10, 9, -3, 4, -2, 10, -9, 8, -5, 7, -6]]
		, [[-1, 10, -2, 1, -4, 5, -10, 2, -6, 8, -7, 9, -3, 4, -5, 3, -9, 6, -8, 7]]
		, [[-1, 10, -2, 1, -3, 9, -10, 2, -7, 8, -4, 5, -9, 3, -5, 4, -6, 7, -8, 6]]
		, [[1, -9, 2, -10, 3, -1, 9, -2, 5, -6, 10, -3, 4, -8, 7, -5, 6, -4, 8, -7]]
		, [[-1, 10, -2, 1, -3, 9, -10, 2, -5, 6, -9, 3, -4, 8, -7, 5, -6, 4, -8, 7]]
		, [[1, -10, 2, -1, 3, -9, 10, -2, 4, -8, 9, -3, 6, -7, 8, -4, 5, -6, 7, -5]]
		, [[-1, 4, -3, 6, -5, 1, -2, 3, -6, 9, -7, 10, -8, 5, -4, 2, -9, 7, -10, 8]]
		, [[-1, 10, -5, 6, -2, 1, -4, 5, -6, 7, -8, 9, -3, 4, -10, 2, -9, 8, -7, 3]]
		, [[1, -10, 2, -1, 5, -9, 7, -3, 10, -2, 3, -6, 8, -7, 4, -5, 6, -8, 9, -4]]
		, [[1, -10, 5, -6, 2, -1, 4, -5, 6, -8, 3, -9, 7, -4, 10, -2, 8, -3, 9, -7]]
		, [[1, -4, 3, -6, 5, -1, 2, -3, 6, -8, 9, -10, 7, -5, 4, -2, 10, -9, 8, -7]]
		, [[1, -10, 2, -1, 3, -6, 5, -7, 10, -2, 7, -9, 8, -3, 4, -5, 9, -8, 6, -4]]
		, [[1, -4, 3, -1, 7, -10, 5, -3, 4, -8, 9, -7, 2, -5, 6, -9, 8, -6, 10, -2]]
		, [[1, -4, 3, -1, 6, -7, 2, -3, 4, -10, 9, -2, 5, -6, 8, -9, 10, -8, 7, -5]]
		, [[1, -2, 8, -10, 4, -1, 2, -7, 9, -8, 5, -6, 3, -4, 7, -9, 10, -3, 6, -5]]
		, [[1, -5, 4, -10, 2, -1, 6, -7, 3, -4, 9, -8, 10, -6, 7, -3, 5, -9, 8, -2]]
		, [[1, -10, 2, -1, 3, -8, 6, -9, 10, -2, 5, -7, 9, -3, 4, -5, 7, -6, 8, -4]]
		, [[1, -10, 6, -7, 2, -1, 3, -9, 5, -6, 7, -8, 4, -5, 10, -2, 8, -4, 9, -3]]
		, [[1, -2, 7, -9, 4, -1, 2, -6, 8, -7, 3, -10, 5, -4, 6, -8, 9, -3, 10, -5]]
		, [[-1, 10, -2, 1, -6, 5, -7, 4, -10, 2, -3, 9, -8, 6, -4, 3, -9, 7, -5, 8]]
		, [[-1, 4, -3, 1, -2, 7, -6, 3, -4, 8, -5, 6, -9, 10, -7, 5, -8, 2, -10, 9]]
		, [[1, -4, 3, -1, 2, -10, 7, -3, 4, -6, 5, -2, 8, -9, 10, -5, 6, -7, 9, -8]]
		, [[-1, 5, -2, 8, -7, 1, -3, 4, -5, 2, -6, 10, -9, 7, -8, 6, -4, 3, -10, 9]]
		, [[1, -9, 2, -10, 8, -1, 4, -5, 9, -2, 3, -7, 6, -4, 5, -3, 10, -8, 7, -6]]
		, [[1, -3, 9, -2, 10, -6, 8, -4, 3, -9, 7, -5, 6, -8, 4, -1, 2, -7, 5, -10]]
		, [[1, -10, 2, -1, 3, -8, 7, -9, 10, -2, 5, -6, 8, -3, 4, -5, 9, -7, 6, -4]]
		, [[1, -4, 3, -7, 5, -1, 6, -8, 2, -3, 7, -10, 9, -5, 4, -2, 8, -6, 10, -9]]
		, [[1, -3, 9, -2, 10, -4, 8, -7, 3, -9, 6, -5, 7, -8, 4, -1, 2, -6, 5, -10]]
		, [[1, -10, 2, -9, 8, -1, 4, -6, 3, -7, 9, -8, 5, -4, 10, -2, 6, -3, 7, -5]]
		, [[1, -10, 2, -1, 4, -5, 3, -8, 7, -6, 10, -2, 8, -9, 5, -4, 6, -7, 9, -3]]
		, [[1, -4, 3, -6, 5, -1, 2, -9, 7, -3, 6, -10, 8, -5, 4, -2, 9, -7, 10, -8]]
		, [[-1, 10, -2, 1, -6, 5, -3, 8, -4, 7, -10, 2, -8, 3, -9, 6, -7, 4, -5, 9]]
		, [[1, -10, 2, -9, 8, -1, 4, -5, 6, -7, 9, -8, 3, -4, 10, -2, 7, -6, 5, -3]]
		, [[1, -9, 2, -10, 8, -1, 4, -5, 9, -2, 3, -7, 10, -8, 6, -4, 5, -3, 7, -6]]
		, [[-1, 7, -3, 8, -4, 1, -2, 5, -6, 3, -7, 10, -9, 6, -8, 4, -5, 9, -10, 2]]
		, [[1, -5, 2, -8, 7, -1, 3, -4, 5, -2, 6, -10, 4, -3, 9, -7, 8, -6, 10, -9]]
		, [[1, -5, 2, -6, 8, -1, 9, -2, 10, -8, 3, -7, 4, -9, 5, -10, 6, -3, 7, -4]]
		, [[1, -10, 2, -1, 3, -7, 5, -6, 10, -2, 8, -5, 9, -3, 4, -8, 6, -9, 7, -4]]
		, [[1, -6, 2, -7, 8, -1, 9, -2, 10, -8, 4, -5, 3, -9, 6, -10, 7, -3, 5, -4]]
		, [[1, -10, 7, -8, 2, -1, 4, -5, 8, -7, 6, -9, 10, -2, 3, -4, 9, -6, 5, -3]]
		, [[1, -9, 2, -10, 6, -1, 3, -4, 9, -5, 10, -8, 7, -3, 4, -2, 5, -6, 8, -7]]
		, [[-1, 10, -6, 7, -2, 1, -4, 5, -8, 6, -7, 9, -3, 4, -10, 2, -9, 8, -5, 3]]
		, [[1, -7, 6, -10, 2, -1, 5, -6, 9, -8, 10, -4, 3, -5, 7, -9, 8, -2, 4, -3]]
		, [[-1, 4, -3, 8, -5, 1, -2, 3, -6, 7, -8, 10, -9, 5, -7, 6, -4, 2, -10, 9]]
		, [[-1, 10, -5, 7, -2, 1, -6, 8, -7, 5, -4, 6, -3, 9, -8, 4, -10, 2, -9, 3]]
		, [[-1, 10, -4, 5, -6, 1, -2, 9, -3, 4, -7, 6, -9, 8, -10, 7, -5, 3, -8, 2]]
		, [[-1, 3, -6, 7, -4, 1, -2, 8, -5, 6, -9, 4, -10, 2, -3, 9, -7, 5, -8, 10]]
		, [[1, -6, 2, -4, 3, -10, 9, -1, 7, -2, 5, -3, 8, -9, 6, -7, 4, -5, 10, -8]]
		, [[1, -10, 2, -1, -4, 5, 10, -2, -3, 8, -6, 9, -7, 4, -5, 3, -8, 6, -9, 7]]
		, [[-1, 10, -2, 1, -3, 9, -10, 2, 5, -7, 6, -8, -9, 3, 4, -5, 7, -6, 8, -4]]
		, [[1, -10, 2, -1, -3, 9, 10, -2, -5, 7, -6, 8, -9, 3, -4, 5, -7, 6, -8, 4]]
		, [[-1, 10, -2, 1, 3, -9, -10, 2, -5, 7, -6, 8, 9, -3, -4, 5, -7, 6, -8, 4]]
		, [[1, -10, 2, -1, -4, 5, 10, -2, -3, 7, -8, 9, -6, 4, -5, 3, -9, 8, -7, 6]]
		, [[-1, 10, -2, 1, -3, 9, -10, 2, 5, -6, 7, -8, -9, 3, 4, -5, 8, -7, 6, -4]]
		, [[1, -10, 2, -1, -3, 9, 10, -2, -5, 6, -7, 8, -9, 3, -4, 5, -8, 7, -6, 4]]
		, [[-1, 10, -2, 1, 3, -9, -10, 2, -5, 6, -7, 8, 9, -3, -4, 5, -8, 7, -6, 4]]
		, [[1, -10, 2, -1, -3, 9, 10, -2, -5, 6, -9, 3, -7, 8, -4, 5, -6, 4, -8, 7]]
		, [[-1, 10, -2, 1, -4, 5, -10, 2, 3, -9, 6, 4, -5, -3, 7, -8, 9, -6, 8, -7]]
		, [[1, -10, 2, -1, -4, 5, 10, -2, -3, 9, -6, 4, -5, 3, -7, 8, -9, 6, -8, 7]]
		, [[-1, 10, -2, 1, 4, -5, -10, 2, -3, 9, -6, -4, 5, 3, -7, 8, -9, 6, -8, 7]]
		, [[-1, -7, 8, -6, 9, -8, 7, -5, 10, -9, 6, 2, -4, 3, 5, -10, -2, 1, -3, 4]]
		, [[-1, 4, -3, 1, -2, -7, 6, 3, -4, 2, 5, -10, 9, -6, 7, -5, 8, -9, 10, -8]]
		, [[1, -8, 10, -9, 8, -5, -7, 6, 9, -10, 5, -2, 4, -3, -6, 7, 2, -1, 3, -4]]
		, [[1, -10, 2, -1, -4, 6, -5, 7, 10, -2, -3, 9, -8, 4, -6, 5, -7, 3, -9, 8]]
		, [[-1, 10, -2, 1, 4, -5, 6, -7, -10, 2, -3, 9, -8, -4, 7, -6, 5, 3, -9, 8]]
		, [[-1, 10, -2, 1, 3, -5, 4, -9, -10, 2, -7, 8, 9, -3, 5, -4, -6, 7, -8, 6]]
		, [[1, -10, 2, -1, -4, 5, -6, 7, 10, -2, -3, 9, -8, 4, -7, 6, -5, 3, -9, 8]]
		, [[1, -10, 2, -1, -3, 5, -4, 9, 10, -2, -7, 8, -9, 3, -5, 4, -6, 7, -8, 6]]
		, [[-1, 10, -2, 1, -4, 5, -6, 7, -10, 2, 3, -9, 8, 4, -7, 6, -5, -3, 9, -8]]
		, [[1, 8, -9, 7, -6, 5, -10, 9, -8, 2, -5, 6, 4, -3, -7, 10, -2, -1, 3, -4]]
		, [[1, -4, 3, -1, -2, 7, -6, -3, 4, 8, -5, 6, -9, 10, -7, 5, -8, 2, -10, 9]]
		, [[1, -10, 2, -1, -3, 7, 5, -6, 10, -2, 8, -5, 9, 3, -4, -8, 6, -9, -7, 4]]
		, [[1, -10, 2, -1, -3, 9, 10, -2, -5, 6, -9, 3, -4, 8, -7, 5, -6, 4, -8, 7]]
		, [[-1, 10, -2, 1, 3, -9, -10, 2, -5, 6, 9, -3, -4, 8, -7, 5, -6, 4, -8, 7]]
		, [[1, -10, 2, -1, -3, 9, 10, -2, -4, 8, -9, 3, -6, 7, -8, 4, -5, 6, -7, 5]]
		, [[-1, 5, -7, 6, -5, 4, -8, 7, -6, -3, 9, 8, -4, 2, -10, -9, 3, 1, -2, 10]]
		, [[-1, 9, -2, 10, -3, 1, -9, 2, 5, -6, -10, 3, 4, -8, 7, -5, 6, -4, 8, -7]]
		, [[1, -10, 2, -1, 3, -9, 10, -2, -5, 6, 9, -3, -4, 8, -7, 5, -6, 4, -8, 7]]
		, [[1, 5, -7, 6, -5, 4, -8, 7, -6, -3, 9, 8, -4, -2, 10, -9, 3, -1, 2, -10]]
		, [[-1, 4, -3, 6, -5, 1, -2, 9, -7, 3, -6, -10, 8, 5, -4, 2, -9, 7, 10, -8]]
		, [[1, -10, 2, -1, 6, -5, -3, 8, 4, -7, 10, -2, -8, 3, 9, -6, 7, -4, 5, -9]]
		, [[-1, 6, -7, -8, 10, -3, -5, 4, -6, 7, 3, -2, 9, 5, -4, 1, 8, -10, 2, -9]]
		, [[1, 5, -2, 8, -7, -1, 3, -4, -5, 2, -6, 10, 4, -3, 9, 7, -8, 6, -10, -9]]
		, [[-1, 5, -2, 6, -8, 1, -9, 2, -10, 8, 3, -7, 4, 9, -5, 10, -6, -3, 7, -4]]
		, [[1, 9, -5, 4, -7, 6, -9, 3, -8, -2, 10, 7, -4, 8, -3, 5, -6, -1, 2, -10]]
		, [[-1, 10, -2, 1, 6, -5, -3, 8, 4, -7, -10, 2, -8, 3, 9, -6, 7, -4, 5, -9]]
		, [[1, -4, 3, -7, 5, -1, 6, -8, 2, -3, 7, 10, -9, -5, 4, -2, 8, -6, -10, 9]]
		, [[1, -6, 2, -7, 8, -1, 9, -2, 10, -8, -4, 5, -3, -9, 6, -10, 7, 3, -5, 4]]
		, [[1, 3, -6, 7, -4, -1, 2, 8, -5, 6, -9, 4, 10, -2, -3, 9, -7, 5, -8, -10]]
		, [[-1, 4, -3, 8, -5, 1, -2, 3, -6, 7, -8, -10, 9, 5, -7, 6, -4, 2, 10, -9]]
		])
	]
