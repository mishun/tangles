#include <cassert>
#include <cstddef>
#include <cstring>
#include <utility>
#include <vector>

namespace Math { namespace Manifolds { namespace SphereStar {

	class DisjointSets
	{
	private:
		struct Node
		{
			Node * father;
			size_t rank;

			Node * findSet()
			{
				if(father != this)
					father = father->findSet();
				return father;
			}
		};

	private:
		Node * const data;
		const size_t _size;
		size_t number_of_sets;

	public:
		DisjointSets(size_t size)
			: data(new Node[size])
			, _size(size)
		{
			clear();
		}

		~DisjointSets()
		{
			delete[] data;
		}

		void clear()
		{
			for(size_t i = 0; i < _size; i++)
			{
				data[i].father = data + i;
				data[i].rank = 0;
			}
			number_of_sets = _size;
		}

		size_t size() const
		{
			return _size;
		}

		size_t numberOfSets() const
		{
			return number_of_sets;
		}

		size_t findSet(size_t index) const
		{
			assert(index < _size);
			return data[index].findSet() - data;
		}

		bool unionSet(size_t a, size_t b)
		{
			assert(a < _size);
			assert(b < _size);

			Node * an = data[a].findSet();
			Node * bn = data[b].findSet();

			if(an == bn)
				return false;

			if(an->rank >= bn->rank)
			{
				bn->father = an;
				if(an->rank == bn->rank)
					an->rank++;
			}
			else
				an->father = bn;

			number_of_sets--;
			return true;
		}
	};


	struct Context
	{
		const size_t numberOfVertices;
		const size_t numberOfFaces;
		const size_t numberOfEdges;
		const size_t * const vertexDegree;
		const size_t (* const edges)[2];
		const size_t (* const coedges)[2];
		size_t * const faceMarks;
		size_t * const edgeMarks;
	};

	static bool testFaceSet(Context & context, std::vector<bool> & mark, const size_t markedFaces)
	{
			std::vector<bool> internal(context.numberOfEdges, false);
			DisjointSets comps(context.numberOfVertices);

			size_t starEdges = 0;
			for(size_t i = 0; i < context.numberOfEdges; i++)
			{
				const size_t l = context.coedges[i][0];
				const size_t r = context.coedges[i][1];

				if(mark[l] && mark[r])
					starEdges++;
				else
				{
					const size_t u = context.edges[i][0];
					const size_t v = context.edges[i][1];

					if(comps.findSet(u) != comps.findSet(v))
					{
						internal[i] = true;
						comps.unionSet(u, v);
					}
				}
			}

			const size_t eulerChar = context.numberOfVertices + context.numberOfFaces - context.numberOfEdges;
			if(comps.numberOfSets() + markedFaces - eulerChar != starEdges)
				return false;

			for(size_t i = 0; i < context.numberOfEdges; i++)
			{
				const size_t u = context.edges[i][0];
				const size_t v = context.edges[i][1];

				if(comps.findSet(u) != comps.findSet(v))
				{
					internal[i] = true;
					comps.unionSet(u, v);
				}
			}

			if(comps.numberOfSets() != 1)
				return false;

			for(size_t i = 0; i < context.numberOfFaces; i++)
				context.faceMarks[i] = (mark[i] ? 1 : 0);

			for(size_t i = 0; i < context.numberOfEdges; i++)
				context.edgeMarks[i] = (internal[i] ? 1 : 0);

			return true;
	}

	static bool searchFacesSubset(Context & context, std::vector<bool> & mark, const size_t facesToMark, const size_t fromIndex, const size_t marksLeft)
	{
			if(marksLeft == 0)
				return testFaceSet(context, mark, facesToMark);

			for(size_t i = fromIndex; i < context.numberOfFaces; i++)
			{
				mark[i] = true;
				if(searchFacesSubset(context, mark, facesToMark, i + 1, marksLeft - 1))
					return true;
				mark[i] = false;
			}

			return false;
	}

	static bool backtrack(Context & context)
	{
		for(size_t splitSize = 0; splitSize <= context.numberOfFaces; splitSize++)
		{
			std::vector<bool> mark(context.numberOfFaces, false);
			if(searchFacesSubset(context, mark, splitSize, 0, splitSize))
				return true;
		}

		return false;
	}

	extern "C" void sphereStarDecomposition
		( const size_t numberOfVertices
		, const size_t numberOfFaces
		, const size_t numberOfEdges
		, const size_t * const vertexDegree
		, const size_t (* const edges)[2]
		, const size_t (* const coedges)[2]
		, size_t * const faceMarks
		, size_t * const edgeMarks
		)
	{
		Context context
			{ .numberOfVertices = numberOfVertices
			, .numberOfFaces    = numberOfFaces
			, .numberOfEdges    = numberOfEdges
			, .vertexDegree     = vertexDegree
			, .edges            = edges
			, .coedges          = coedges
			, .faceMarks        = faceMarks
			, .edgeMarks        = edgeMarks
			};

		const bool ok = backtrack(context);
		assert(ok && "Can not find solution");
	}

}}}
