#include <cassert>
#include <cstddef>
#include <vector>

namespace Math { namespace Manifolds { namespace SphereStar {

#if 0
	class SphereStarDecompCalculator
	{
	protected:
		typedef std::vector< std::vector< std::pair<size_t, size_t> > > Representation;

	protected:
		const EmbeddedGraph & graph;
		std::pair< EmbeddedGraph *, EmbeddedGraph * > result;

	public:
		SphereStarDecompCalculator(const EmbeddedGraph &);
		std::pair< EmbeddedGraph *, EmbeddedGraph * > decomposition();

	protected:
		size_t simplifyTree(Representation &, std::vector<bool> &) const;
		void edgeToSphere(Representation &, size_t) const;
		void edgeToBorder(Representation &, size_t, size_t) const;
		void generateStar(Representation &, const Representation &, const std::vector<bool> &) const;
		bool searchFacesSubset(bool[], size_t, size_t, size_t);
		bool testFaceSet(bool[], size_t);
	};

SphereStarDecompCalculator::SphereStarDecompCalculator(const EmbeddedGraph & g)
	: graph(g)
{
	assert(graph.genus() > 0);
}

std::pair< EmbeddedGraph *, EmbeddedGraph * > SphereStarDecompCalculator::decomposition()
{
	for(size_t i = 1; i <= graph.numberOfFaces(); i++)
	{
		Util::AutoArray<bool> mark(graph.numberOfFaces(), false);
		if(searchFacesSubset(mark.get(), 0, i, i))
			break;
	}

	return result;
}

size_t SphereStarDecompCalculator::simplifyTree(Representation & sphere, std::vector<bool> & internal) const
{
	{
		const EmbeddedGraph::Edge * prev_edge = 0;
		for(size_t i = 0; i < graph.numberOfEdges(); i++)
			if(!internal[i])
			{
				prev_edge = &graph.edge(i);
				break;
			}
		assert(prev_edge != 0);

		size_t prev_side = 0;
		std::vector<size_t> external_stack;
		external_stack.reserve(4 * graph.numberOfEdges());

		for(size_t assigned_edges = 2 * (graph.numberOfVertexes() - 1); assigned_edges < 2 * graph.numberOfEdges(); )
		{
			const EmbeddedGraph::Vertex & u = prev_edge->incidentVertex(prev_side).vertex;
			const size_t prev_place = prev_edge->incidentVertex(prev_side).place;

			const size_t u_place = u.nextIndexCW(prev_place);
			const EmbeddedGraph::Edge & e = u.incidentEdge(u_place).edge;
			const size_t side = u.incidentEdge(u_place).place;

			if(internal[e.idInGraph()])
			{
				edgeToSphere(sphere, e.idInGraph());
				prev_side = side ^ 1;
			}
			else
			{
				if(external_stack.size() <= 2 * graph.numberOfEdges())
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
	}

	{
		size_t weight = 0;
		for(size_t i = 0; i < graph.numberOfEdges(); i++)
			if(!internal[i])
				weight++;

		return weight;
	}
}

void SphereStarDecompCalculator::edgeToSphere(Representation & sphere, size_t eid) const
{
	const EmbeddedGraph::Edge & e = graph.edge(eid);

	const size_t u = e.begin().vertex.idInGraph();
	const size_t u_place = e.begin().place;
	const size_t v = e.end().vertex.idInGraph();
	const size_t v_place = e.end().place;

	sphere[u + 1] [u_place] = std::make_pair(v + 1, v_place);
	sphere[v + 1] [v_place] = std::make_pair(u + 1, u_place);
}

void SphereStarDecompCalculator::edgeToBorder(Representation & sphere, size_t eid, size_t side) const
{
	const EmbeddedGraph::Edge & e = graph.edge(eid);

	const size_t u = e.incidentVertex(side).vertex.idInGraph();
	const size_t u_place = e.incidentVertex(side).place;
	const size_t cur_index = sphere[0].size();

	sphere[u + 1] [u_place] = std::make_pair(0, cur_index);
	sphere[0].push_back(std::make_pair(u + 1, u_place));
}

void SphereStarDecompCalculator::generateStar(Representation & star, const Representation & sphere, const std::vector<bool> & internal) const
{
	star[0].resize(sphere[0].size());
	for(size_t i = 0; i < graph.numberOfEdges(); i++)
		if(!internal[i])
		{
			const EmbeddedGraph::Edge & e = graph.edge(i);
			const size_t u = sphere[e.begin().vertex.idInGraph() + 1] [e.begin().place].second;
			const size_t v = sphere[e.end().vertex.idInGraph() + 1] [e.end().place].second;
			star[0] [u] = std::make_pair(0, v);
			star[0] [v] = std::make_pair(0, u);
		}
}




bool SphereStarDecompCalculator::searchFacesSubset(bool mark[], size_t from, size_t left, size_t faces)
{
	if(left == 0)
		return testFaceSet(mark, faces);

	if(from >= graph.numberOfFaces())
		return false;

	for(size_t i = from; i < graph.numberOfFaces(); i++)
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
	std::vector<bool> internal(graph.numberOfEdges(), false);

	Util::DisjointSets comps(graph.numberOfVertexes());

	size_t edges = 0;
	for(size_t i = 0; i < graph.numberOfEdges(); i++)
	{
		const size_t l = graph.edge(i).left().face.idInGraph();
		const size_t r = graph.edge(i).right().face.idInGraph();

		if(mark[l] && mark[r])
		{
			edges++;
			continue;
		}

		const size_t u = graph.edge(i).begin().vertex.idInGraph();
		const size_t v = graph.edge(i).end().vertex.idInGraph();

		if(comps.findSet(u) != comps.findSet(v))
		{
			internal[i] = true;
			comps.unionSet(u, v);
		}
	}

	if(comps.numberOfSets() + faces + 2 * graph.genus() == 2 + edges)
	{
		for(size_t i = 0; i < graph.numberOfEdges(); i++)
		{
			const size_t u = graph.edge(i).begin().vertex.idInGraph();
			const size_t v = graph.edge(i).end().vertex.idInGraph();

			if(comps.findSet(u) != comps.findSet(v))
			{
				internal[i] = true;
				comps.unionSet(u, v);
			}
		}

		std::vector< std::vector< std::pair<size_t, size_t> > > sphere(graph.numberOfVertexes() + 1);
		for(size_t i = 0; i < graph.numberOfVertexes(); i++)
			sphere[i + 1].resize(graph.vertex(i).numberOfIncidentEdges());


		size_t cur = simplifyTree(sphere, internal);

		assert(cur == faces + 2 * graph.genus() - 1);

		std::vector< std::vector< std::pair<size_t, size_t> > > star(1);
		generateStar(star, sphere, internal);

		EmbeddedGraph & sphere_graph = EmbeddedGraph::createFromVertexAdjacencyList(sphere);
		EmbeddedGraph & star_graph = EmbeddedGraph::createFromVertexAdjacencyList(star);

		assert(sphere_graph.genus() == 0);
		assert(star_graph.genus() == graph.genus());

		result = std::make_pair(&sphere_graph, &star_graph);
		return true;
	}
	else
		return false;
}
#endif

}}}
