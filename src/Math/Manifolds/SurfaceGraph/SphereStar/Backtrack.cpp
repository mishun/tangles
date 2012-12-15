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


	struct SphereStarContext
	{
		const size_t numberOfVertices;
		const size_t numberOfFaces;
		const size_t numberOfEdges;
		const size_t * const vertexDegree;
	};


	class SphereStarDecompCalculator
	{
	protected:
		typedef std::vector< std::vector< std::pair<size_t, size_t> > > Representation;

	protected:
		const SphereStarContext & context;
		//const EmbeddedGraph & graph;
		//std::pair< EmbeddedGraph *, EmbeddedGraph * > result;

	public:
		SphereStarDecompCalculator(const SphereStarContext &);
		void decomposition();

	protected:
		size_t simplifyTree(Representation &, std::vector<bool> &) const;
		void edgeToSphere(Representation &, size_t) const;
		void edgeToBorder(Representation &, size_t, size_t) const;
		void generateStar(Representation &, const Representation &, const std::vector<bool> &) const;
		bool searchFacesSubset(bool[], size_t, size_t, size_t);
		bool testFaceSet(bool[], size_t);
	};

	SphereStarDecompCalculator::SphereStarDecompCalculator(const SphereStarContext & c)
		: context(c)
	{
	}


	void sphereStarDecomposition
		( const size_t numberOfVertices
		, const size_t numberOfFaces
		, const size_t numberOfEdges
		, const size_t * const vertexDegree
		)
	{
		SphereStarContext context
			{ .numberOfVertices = numberOfVertices
			, .numberOfFaces    = numberOfFaces
			, .numberOfEdges    = numberOfEdges
			, .vertexDegree     = vertexDegree
			};

		SphereStarDecompCalculator calc(context);
		calc.decomposition();
	}



	void SphereStarDecompCalculator::decomposition()
	{
		bool * const mark = new bool[context.numberOfFaces];
		for(size_t i = 0; i < context.numberOfFaces; i++)
		{
			std::memset(mark, 0, sizeof(bool[context.numberOfFaces]));
			if(searchFacesSubset(mark, 0, i, i))
				break;
		}
		delete[] mark;

		//return result;
	}

	size_t SphereStarDecompCalculator::simplifyTree(Representation & sphere, std::vector<bool> & internal) const
	{
		/*{
			const EmbeddedGraph::Edge * prev_edge = nullptr;
			for(size_t i = 0; i < context.numberOfEdges; i++)
				if(!internal[i])
				{
					//prev_edge = &graph.edge(i);
					break;
				}

			//assert(prev_edge != nullptr);

			size_t prev_side = 0;
			std::vector<size_t> external_stack;
			external_stack.reserve(4 * context.numberOfEdges);

			for(size_t assigned_edges = 2 * (context.numberOfVertices - 1); assigned_edges < 2 * context.numberOfEdges; )
			{
				const auto & u = prev_edge->incidentVertex(prev_side).vertex;
				const size_t prev_place = prev_edge->incidentVertex(prev_side).place;

				const size_t u_place = u.nextIndexCW(prev_place);
				const auto & e = u.incidentEdge(u_place).edge;
				const size_t side = u.incidentEdge(u_place).place;

				if(internal[e.idInGraph()])
				{
					edgeToSphere(sphere, e.idInGraph());
					prev_side = side ^ 1;
				}
				else
				{
					if(external_stack.size() <= 2 * context.numberOfEdges)
					{
						if(!external_stack.empty() && e.idInGraph() == external_stack.back())
						{
							internal[e.idInGraph()] = true;
							edgeToSphere(sphere, e.idInGraph());
							assigned_edges += 2;
							external_stack.pop_back();
						}
						else
							external_stack.push_back(e.idInGraph());
					}
					else
					{
						edgeToBorder(sphere, e.idInGraph(), side);
						assigned_edges++;
					}

					prev_side = side;
				}

				prev_edge = &e;
			}
		}*/

		{
			size_t weight = 0;
			for(size_t i = 0; i < context.numberOfEdges; i++)
				if(!internal[i])
					weight++;

			return weight;
		}
	}

	void SphereStarDecompCalculator::edgeToSphere(Representation & sphere, size_t eid) const
	{
		//const auto & e = graph.edge(eid);

		const size_t u = 0;//e.begin().vertex.idInGraph();
		const size_t u_place = 0;//e.begin().place;
		const size_t v = 0;//e.end().vertex.idInGraph();
		const size_t v_place = 0;//e.end().place;

		sphere[u + 1] [u_place] = std::make_pair(v + 1, v_place);
		sphere[v + 1] [v_place] = std::make_pair(u + 1, u_place);
	}

	void SphereStarDecompCalculator::edgeToBorder(Representation & sphere, size_t eid, size_t side) const
	{
		//const auto & e = graph.edge(eid);

		const size_t u = 0;//e.incidentVertex(side).vertex.idInGraph();
		const size_t u_place = 0;//e.incidentVertex(side).place;
		const size_t cur_index = sphere[0].size();

		sphere[u + 1] [u_place] = std::make_pair(0, cur_index);
		sphere[0].push_back(std::make_pair(u + 1, u_place));
	}

	void SphereStarDecompCalculator::generateStar(Representation & star, const Representation & sphere, const std::vector<bool> & internal) const
	{
		star[0].resize(sphere[0].size());
		for(size_t i = 0; i < context.numberOfEdges; i++)
			if(!internal[i])
			{
				//const auto & e = graph.edge(i);
				const size_t u = 0;//sphere[e.begin().vertex.idInGraph() + 1] [e.begin().place].second;
				const size_t v = 0;//sphere[e.end().vertex.idInGraph() + 1] [e.end().place].second;
				star[0] [u] = std::make_pair(0, v);
				star[0] [v] = std::make_pair(0, u);
			}
	}




	bool SphereStarDecompCalculator::searchFacesSubset(bool mark[], size_t from, size_t left, size_t faces)
	{
		if(left == 0)
			return testFaceSet(mark, faces);

		if(from >= context.numberOfFaces)
			return false;

		for(size_t i = from; i < context.numberOfFaces; i++)
		{
			mark[i] = true;
			if(searchFacesSubset(mark, i + 1, left - 1, faces))
				return true;
			mark[i] = false;
		}

		return false;
	}

	bool SphereStarDecompCalculator::testFaceSet(bool mark[], size_t faces)
	{
		std::vector<bool> internal(context.numberOfEdges, false);

		DisjointSets comps(context.numberOfVertices);

		size_t edges = 0;
		for(size_t i = 0; i < context.numberOfEdges; i++)
		{
			const size_t l = 0;//graph.edge(i).left().face.idInGraph();
			const size_t r = 0;//graph.edge(i).right().face.idInGraph();

			if(mark[l] && mark[r])
			{
				edges++;
				continue;
			}

			const size_t u = 0;//graph.edge(i).begin().vertex.idInGraph();
			const size_t v = 0;//graph.edge(i).end().vertex.idInGraph();

			if(comps.findSet(u) != comps.findSet(v))
			{
				internal[i] = true;
				comps.unionSet(u, v);
			}
		}

		const size_t eulerChar = context.numberOfVertices + context.numberOfFaces - context.numberOfEdges;
		if(comps.numberOfSets() + faces - eulerChar == edges)
		{
			for(size_t i = 0; i < context.numberOfEdges; i++)
			{
				const size_t u = 0;//graph.edge(i).begin().vertex.idInGraph();
				const size_t v = 0;//graph.edge(i).end().vertex.idInGraph();

				if(comps.findSet(u) != comps.findSet(v))
				{
					internal[i] = true;
					comps.unionSet(u, v);
				}
			}

			std::vector< std::vector< std::pair<size_t, size_t> > > sphere(context.numberOfVertices + 1);
			for(size_t i = 0; i < context.numberOfVertices; i++)
				sphere[i + 1].resize(context.vertexDegree[i]);


			/*size_t cur = */ simplifyTree(sphere, internal);

			//assert(cur == faces + 2 * graph.genus() - 1);

			std::vector< std::vector< std::pair<size_t, size_t> > > star(1);
			generateStar(star, sphere, internal);

			//EmbeddedGraph & sphere_graph = EmbeddedGraph::createFromVertexAdjacencyList(sphere);
			//EmbeddedGraph & star_graph = EmbeddedGraph::createFromVertexAdjacencyList(star);
			//assert(sphere_graph.genus() == 0);
			//assert(star_graph.genus() == graph.genus());
			//result = std::make_pair(&sphere_graph, &star_graph);

			return true;
		}
		else
			return false;
	}

}}}
