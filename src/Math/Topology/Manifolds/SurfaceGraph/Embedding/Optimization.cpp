#include <cassert>
#include <cmath>
#include <cstddef>
#include <algorithm>
#include <gsl/gsl_blas.h>
#include <gsl/gsl_multimin.h>
#include <Math/Numeric/Vector2.h>
#include <Math/Topology/Manifolds/SurfaceGraph/Embedding/Optimization.h>


namespace Math { namespace Manifolds { namespace Embedding { namespace Optimization {

	using Math::Numeric::Vector2;

	namespace Initialization {

		inline double dot(const size_t n, const double * x, const double * y)
		{
			double dot = 0.0;
			for(size_t i = 0; i < n; i++)
				dot += x[i] * y[i];
			return dot;
		}

		inline void multiply(const size_t n
			, const size_t m
			, const size_t * id
			, const double * a
			, const double * x
			, double * r)
		{
			for(size_t i = 0; i < n; i++)
				r[i] = 0.0;

			for(size_t i = 0; i < m; i++)
			{
				const size_t p = id[2 * i];
				const size_t q = id[2 * i + 1];
				r[p] += a[i] * x[q];
			}
		}

		inline void diff(const size_t n
			, const size_t m
			, const size_t * id
			, const double * a
			, const double * b
			, const double * x
			, double * r)
		{
			multiply(n, m, id, a, x, r);
			for(size_t i = 0; i < n; i++)
				r[i] = b[i] - r[i];
		}

		extern "C" double conjugateGradientSolve
			( const size_t n
			, const size_t m
			, const size_t * id
			, const double * a
			, const double * b
			, double * x
			)
		{
			double * r = new double[n];
			diff(n, m, id, a, b, x, r);

			double * d = new double[n];
			for(size_t i = 0; i < n; i++)
				d[i] = r[i];

			double * q = new double[n];

			double delta_new = dot(n, r, r);
			const double delta0 = delta_new;
			const double eps = 1e-14;

			for(size_t k = 0; k < n && delta_new > eps * eps * delta0; k++)
			{
				multiply(n, m, id, a, d, q);
				double alpha = delta_new / dot(n, d, q);

				for(size_t i = 0; i < n; i++)
					x[i] += alpha * d[i];

				if(k % 50 == 0)
					diff(n, m, id, a, b, x, r);
				else
				{
					for(size_t i = 0; i < n; i++)
						r[i] -= alpha * q[i];
				}

				const double delta_old = delta_new;
				delta_new = dot(n, r, r);

				{
					const double beta = delta_new / delta_old;
					for(size_t i = 0; i < n; i++)
						d[i] = r[i] + beta * d[i];
				}
			}

			delete[] r;
			delete[] d;
			delete[] q;

			return delta_new;
		}
	}


	namespace Relax {

		struct InteractionContext
		{
			const InteractionConst & interaction;

			const size_t numberOfBorderSegments;
			const size_t totalNumberOfVertices;
			const size_t numberOfMovableVertices;
			const size_t numberOfFrozenVertices;

			const size_t numberOfThreads;
			const size_t * const lengthOfThread;
			const size_t * const * const threads;

			const size_t numberOfCrossings;
			const size_t * const crossingDegree;
			const size_t * const * const crossingAdjacencyList;
		};

		static double energy(const InteractionContext & context, const Vector2 * x, Vector2 * grad)
		{
			double phi = 0.0;
			for(size_t i = 0; i < context.totalNumberOfVertices; i++)
				grad[i].y = grad[i].x = 0.0;

			// Border interaction energy
			if(context.interaction.border > 0.0)
			{
				if(context.numberOfBorderSegments < 3)
				{
					const double k = context.interaction.border;
					for(size_t i = 0; i < context.numberOfMovableVertices; i++)
					{
						const double r = x[i].length();
						phi -= k * r * r * log(1.0 - r);
						grad[i] -= x[i] * (k * (2.0 * log(1.0 - r) - r / (1.0 - r)));
					}
				}
				else
				{
					const double k = context.interaction.border;
					const double halfAngle = M_PI / context.numberOfBorderSegments;
					for(size_t j = 0; j < context.numberOfBorderSegments; j++)
					{
						const Vector2 a = Vector2::polar(2.0 * halfAngle * j - halfAngle);
						const Vector2 b = Vector2::polar(2.0 * halfAngle * j + halfAngle);
						const auto ab = b - a;
						const double s = ab.length();

						for(size_t i = 0; i < context.numberOfMovableVertices; i++)
						{
							const auto aj = x[i] - a;
							const auto bj = x[i] - b;
							const double r1 = aj.length();
							const double r2 = bj.length();
							const double d = r1 + r2;

							phi += k * log((d + s) / (d - s));

							const double f = 2.0 * k / (d * d - s * s);
							const auto gaj = aj * (-f * s / r1);
							const auto gbj = bj * (-f * s / r2);

							grad[i] += gaj + gbj;
						}
					}
				}
			}

			for(size_t ti = 0; ti < context.numberOfThreads; ti++)
			{
				const size_t len = context.lengthOfThread[ti];
				const size_t * const thread = context.threads[ti];
				const bool closed = (thread[0] == thread[len - 1]);

				for(size_t i = 0; i + 1 < len; i++)
				{
					const size_t a = thread[i];
					const size_t b = thread[i + 1];
					const auto ab = x[b] - x[a];
					const double s = ab.length();

					// Elastic energy (a -- b)
					// E = \frac{k ab^2}{2}
					{
						const double k = context.interaction.elastic;
						phi += 0.5 * k * s * s;
						grad[a] -= ab * k;
						grad[b] += ab * k;
					}

					// Electric energy (a -- b  >  j)
					// E = k * \ln\frac{r1 + r2 + s}{r1 + r2 - s}
					if(context.interaction.electric > 0.0)
					{
						const double k = context.interaction.electric;
						for(size_t j = 0; j < context.numberOfMovableVertices; j++)
						{
							if(j == a || j == b)
								continue;

							const auto aj = x[j] - x[a];
							const auto bj = x[j] - x[b];
							const double r1 = aj.length();
							const double r2 = bj.length();
							const double d = r1 + r2;

							phi += k * log((d + s) / (d - s));

							const double f = 2.0 * k / (d * d - s * s);
							const auto gaj = aj * (-f * s / r1);
							const auto gbj = bj * (-f * s / r2);
							const auto gab = ab * (f * d / s);

							grad[j] += gaj + gbj;
							grad[a] -= gaj + gab;
							grad[b] -= gbj - gab;
						}
					}

					// Bend energy
					/*{
						const double k = context.interaction.bend;

						{
							const size_t c = (i > 0) ? thread[i - 1] : (closed ? thread[len - 2] : b);
							const auto df = x[b] + x[c] - x[a] * 2.0;
							phi += 0.25 * k * (df * df);
							grad[b] += (x[b] * 3.0 - x[a] * 4.0 + x[c]) * k;
						}

						{
							const size_t c = (i + 2 < len) ? thread[i + 2] : (closed ? thread[1] : a);
							const auto df = x[a] + x[c] - x[b] * 2.0;
							phi += 0.25 * k * (df * df);
							grad[a] += (x[a] * 3.0 - x[b] * 4.0 + x[c]) * k;
						}
					}*/

					if(closed || i + 2 < len)
					{
						const double k = 0.05 * context.interaction.bend;
						const size_t c = ((i + 2 < len) ? thread[i + 2] : thread[1]);

						// Bend energy (a -- b -- c)
						{
							const auto l = x[a] - x[b];
							const auto r = x[c] - x[b];
							const auto d = r - l;

							const double cross = (l ^ r);
							const double ll = l.length();
							const double rl = r.length();
							const double sl = ll + rl;
							const double A = (k * sl * cross) / (l.length2() * r.length2() * d.length2());

							const double dedlx = A * ((l.x / (ll * sl) - 2.0 * (l.x - r.x) / d.length2() - 2.0 * l.x / l.length2()) * cross + 2.0 * r.y);
							const double dedly = A * ((l.y / (ll * sl) - 2.0 * (l.y - r.y) / d.length2() - 2.0 * l.y / l.length2()) * cross - 2.0 * r.x);
							const double dedrx = A * ((r.x / (rl * sl) - 2.0 * (r.x - l.x) / d.length2() - 2.0 * r.x / r.length2()) * cross - 2.0 * l.y);
							const double dedry = A * ((r.y / (rl * sl) - 2.0 * (r.y - l.y) / d.length2() - 2.0 * r.y / r.length2()) * cross + 2.0 * l.x);

							phi += A * cross;
							grad[a].x += dedlx;
							grad[a].y += dedly;
							grad[c].x += dedrx;
							grad[c].y += dedry;
							grad[b].x -= dedlx + dedrx;
							grad[b].y -= dedly + dedry;
						}
					}
				}
			}

			// Crossing energy
			if(context.interaction.cross > 0.0)
			{
				const double k = 2 * context.interaction.cross;
				for(size_t c = 0; c < context.numberOfCrossings; c++)
				{
					const size_t degree = context.crossingDegree[c];
					const size_t * const adj = context.crossingAdjacencyList[c];
					const double targetAngle = 2.0 * M_PI / degree;

					for(size_t i = 0; i < degree; i++)
					{
						const size_t a = adj[i];
						const size_t b = adj[ i == degree - 1 ? 0 : i + 1 ];

						const auto l = x[a] - x[c];
						const auto r = x[b] - x[c];

						const double cosD = (l * r) / (l.length() * r.length());
						const double delta = acos(std::min(1.0, std::max(-1.0, cosD))) - targetAngle;

						phi += k * 0.5 * delta * delta;

						const auto gl = l.ort() * (-k * delta / l.length2());
						const auto gr = r.ort() * ( k * delta / r.length2());

						grad[a] += gl;
						grad[b] += gr;
						grad[c] -= gl + gr;
					}
				}
			}

			for(size_t i = context.numberOfMovableVertices; i < context.totalNumberOfVertices; i++)
				grad[i] = 0.0;

			return phi;
		}

		static double stepLimitByGrad(const InteractionContext & context, const Vector2 * x, const Vector2 * grad)
		{
			double * dist = new double[context.totalNumberOfVertices];

			for(size_t i = 0; i < context.numberOfMovableVertices; i++)
				dist[i] = 1.0 - x[i].length();

			if(context.numberOfBorderSegments >= 3)
			{
				const double halfAngle = M_PI / context.numberOfBorderSegments;
				for(size_t j = 0; j < context.numberOfBorderSegments; j++)
				{
					const Vector2 a = Vector2::polar(halfAngle * j - halfAngle);
					const Vector2 b = Vector2::polar(halfAngle * j + halfAngle);
					const auto ab = b - a;
					const double abl = ab.length();

					for(size_t i = 0; i < context.numberOfMovableVertices; i++)
					{
						const auto aj = x[i] - a;
						const auto bj = x[i] - b;
						const double r1 = aj.length();
						const double r2 = bj.length();

						const double dp = ab * aj;
						const double d = (dp <= 0.0) ? r1 : ((dp >= abl * abl) ? r2 : fabs(ab ^ aj) / abl);
						dist[i] = std::min(dist[i], d);
					}
				}
			}

			for(size_t ti = 0; ti < context.numberOfThreads; ti++)
			{
				const size_t len = context.lengthOfThread[ti];
				const size_t * const thread = context.threads[ti];

				for(size_t i = 0; i + 1 < len; i++)
				{
					const size_t a = thread[i];
					const size_t b = thread[i + 1];
					const auto ab = x[b] - x[a];
					const double abl = ab.length();

					for(size_t j = 0; j < context.numberOfMovableVertices; j++)
					{
						if(j == a || j == b)
							continue;

						const auto aj = x[j] - x[a];
						const auto bj = x[j] - x[b];
						const double r1 = aj.length();
						const double r2 = bj.length();

						const double dp = ab * aj;
						const double d = (dp <= 0.0) ? r1 : ((dp >= abl * abl) ? r2 : fabs(ab ^ aj) / abl);
						dist[j] = std::min(dist[j], d);
					}
				}
			}

			double limit = std::numeric_limits<double>::infinity();
			for(size_t i = 0; i < context.numberOfMovableVertices; i++)
				limit = std::min(limit, dist[i] / grad[i].length());

			delete[] dist;
			return limit;
		}

		static double stepLimit(const InteractionContext & context, const Vector2 * x)
		{
			auto * const grad = new Vector2[context.totalNumberOfVertices];
			energy(context, x, grad);
			const double limit = stepLimitByGrad(context, x, grad);
			delete[] grad;
			return limit;
		}

		static bool gslMinimization
			( const double eps
			, const size_t maxIterations
			, const gsl_multimin_fdfminimizer_type * const method
			, InteractionContext & context
			, Vector2 * x
			)
		{
			struct Phi
			{
				static inline double callEnergy(const gsl_vector * vx, void * _context, gsl_vector * vg)
				{
					auto & context = *reinterpret_cast<const InteractionContext *>(_context);
					const auto * x = reinterpret_cast<const Vector2 *>(gsl_vector_const_ptr(vx, 0));

					if(vg != nullptr)
						return energy(context, x, reinterpret_cast<Vector2 *>(gsl_vector_ptr(vg, 0)));
					else
					{
						auto * g = new Vector2[context.totalNumberOfVertices];
						const double e = energy(context, x, g);
						delete[] g;
						return e;
					}
				}

				static void phiAndGradPhi(const gsl_vector * x, void * context, double * phi, gsl_vector * g)
				{
					*phi = callEnergy(x, context, g);
				}

				static double phi(const gsl_vector * x, void * context)
				{
					return callEnergy(x, context, nullptr);
				}

				static void gradPhi(const gsl_vector * x, void * context, gsl_vector * g)
				{
					callEnergy(x, context, g);
				}
			};

			gsl_multimin_function_fdf f
				{ .f      = Phi::phi
				, .df     = Phi::gradPhi
				, .fdf    = Phi::phiAndGradPhi
				, .n      = 2 * context.totalNumberOfVertices
				, .params = &context
				};

			auto vx = gsl_vector_view_array(reinterpret_cast<double *>(x), 2 * context.totalNumberOfVertices);
			auto * const s = gsl_multimin_fdfminimizer_alloc(method, 2 * context.totalNumberOfVertices);
			gsl_multimin_fdfminimizer_set(s, &f, &vx.vector, 0.1 * stepLimit(context, x), 0.1);

			const double initialNorm = gsl_blas_dnrm2(s->gradient);
			for(size_t iteration = 1; ; iteration++)
			{
				const auto iterationRes = gsl_multimin_fdfminimizer_iterate(s);

				const double phi = s->f;
				const double gnorm = gsl_blas_dnrm2(s->gradient);
				const double dxnorm = gsl_blas_dnrm2(s->dx);

				if(std::isnan(phi) || std::isnan(gnorm) || std::isnan(dxnorm))
				{
					//	std::cerr
					//		<< gsl_multimin_fdfminimizer_name(s) << ": "
					//		<< "NaN error at iteration = " << iteration
					//		<< " phi = " << phi
					//		<< " |dx| = " << dxnorm << "\n";

					gsl_multimin_fdfminimizer_free(s);
					return false;
				}

				if(iterationRes != GSL_SUCCESS || iteration >= maxIterations || gnorm <= eps * initialNorm)
				{
					//	std::cerr
					//		<< gsl_multimin_fdfminimizer_name(s) << ": "
					//		<< (iterationRes == GSL_SUCCESS ? "success" : "GSL_ENOPROG error")
					//		<< " at iteration = " << iteration
					//		<< " phi = " << phi
					//		<< " |g| = " << gnorm << " of " << initialNorm
					//		<< " |dx| = " << dxnorm << "\n";

					gsl_vector_memcpy(&vx.vector, s->x);
					gsl_multimin_fdfminimizer_free(s);
					return iterationRes == GSL_SUCCESS;
				}
			}
		}

		extern "C" void relaxEmbedding
			( const InteractionConst & interaction, const size_t numberOfBorderSegments
			, const size_t numberOfMovableVertices, const size_t numberOfFrozenVertices, Vector2 * x
			, const size_t numberOfThreads, const size_t * const lengthOfThread, const size_t * const * const threads
			, const size_t numberOfCrossings, const size_t * const crossingDegree, const size_t * const * const crossingAdjacencyList
			)
		{
			InteractionContext context
				{ .interaction             = interaction
				, .numberOfBorderSegments  = numberOfBorderSegments
				, .totalNumberOfVertices   = numberOfMovableVertices + numberOfFrozenVertices
				, .numberOfMovableVertices = numberOfMovableVertices
				, .numberOfFrozenVertices  = numberOfFrozenVertices
				, .numberOfThreads         = numberOfThreads
				, .lengthOfThread          = lengthOfThread
				, .threads                 = threads
				, .numberOfCrossings       = numberOfCrossings
				, .crossingDegree          = crossingDegree
				, .crossingAdjacencyList   = crossingAdjacencyList
				};

			const double eps = 1e-3;
			const size_t maxIterations = 2048;

			if(!gslMinimization(eps, maxIterations, gsl_multimin_fdfminimizer_conjugate_fr, context, x))
				gslMinimization(eps, maxIterations, gsl_multimin_fdfminimizer_steepest_descent, context, x);
		}

	}


	namespace CirclePacking {

		inline double externalCentralAngle(const double r, const double r1, const double r2)
		{
			const double a = r + r1;
			const double b = r + r2;
			const double c = r1 + r2;
			return acos((a * a + b * b - c * c) / (2.0 * a * b));
		}

		inline double internalCentralAngle(const double r, const double r1, const double r2)
		{
			const double a = r - r1;
			const double b = r - r2;
			const double c = r1 + r2;

			if(a > 0.0 && b > 0.0 && c > 0.0 && c <= a + b && b <= a + c && a <= b + c)
				return acos((a * a + b * b - c * c) / (2.0 * a * b));
			else
				return std::numeric_limits<double>::infinity();
		}

		inline double crossCentralAngle(const double r, const double r1, const double r2)
		{
			double a = r1 - r;
			double b = r + r2;
			double c = r1 - r2;

			if(a > 0.0 && b > 0.0 && c > 0.0 && c <= a + b && b <= a + c && a <= b + c)
				return acos(-(a * a + b * b - c * c) / (2 * a * b));
			else
				return -std::numeric_limits<double>::infinity();
		}

		struct TangentContext
		{
			const size_t numberOfVertices;
			const size_t * const vertexDegree;
			const size_t * const * adjacencyList;
		};

		static void circlePackingSolve(const TangentContext & context, double * r)
		{
			for(size_t i = 0; i < context.numberOfVertices; i++)
				r[i] = 1.0;
		}

		extern "C" void circlePacking
			( const size_t numberOfVertices
			, const size_t * const vertexDegree
			, const size_t * const * const adjacencyList
			, double * r
			)
		{
			TangentContext context
				{ .numberOfVertices = numberOfVertices
				, .vertexDegree     = vertexDegree
				, .adjacencyList    = adjacencyList
				};

			circlePackingSolve(context, r);
		}

	}


#if 0
class TangentDrawer
{
public:
	TangentDrawer(const std::vector< std::vector<size_t> > & v, const std::vector<size_t> & e)
		: g(e.size() + 2)
		, precision(1e-6)
	{
		const size_t e_base = v.size(), f_base = v.size() + e.size() / 2 - 1;
		type.assign(g.size(), 0);

		std::vector<size_t> face(e.size(), 0);
		size_t face_id = 1;
		for(size_t i = 0; i < e.size(); i++)
		{
			if(face[i] > 0)
				continue;

			size_t cur_face = face_id++;
			for(size_t j = i; face[j] == 0; )
			{
				face[j] = cur_face;
				g[f_base + cur_face].push_back(e_base + j / 2);
				size_t vertex = e[j];
				g[f_base + cur_face].push_back(vertex);
				size_t off = 0;
				while(v[vertex] [off] != j) off++;
				off = (off + 1) % v[vertex].size();
				j = v[vertex] [off] ^ 1;
			}
			std::reverse(g[f_base + cur_face].begin(), g[f_base + cur_face].end());
			type[f_base + cur_face] = 2;
		}

		for(size_t i = 0; i < v.size(); i++)
			for(size_t j = 0; j < v[i].size(); j++)
			{
				g[i].push_back(e_base + v[i] [j] / 2);
				g[i].push_back(f_base + face[ v[i] [j] ]);
			}

		for(size_t i = 0; i < e.size(); i += 2)
		{
			g[e_base + i / 2].push_back(f_base + face[i]);
			g[e_base + i / 2].push_back(e[i]);
			g[e_base + i / 2].push_back(f_base + face[i + 1]);
			g[e_base + i / 2].push_back(e[i + 1]);
			type[e_base + i / 2] = 1;
		}

		deformation = false;
	}

	std::string render()
	{
		if(searchRad() >= precision)
			throw std::logic_error("can not find solution");

		getPositions();

		for(size_t j = 0; j < 20; j++)
		{
			Vector2D center(0, 0);
			double mass = 0;

			for(size_t i = 1; i < g.size(); i++)
			{
				if(type[i] != 0)
					continue;

				double m = 1.0 / r[i];
				center += x[i] * m;
				mass += m;
			}

			move(center / mass);
		}

		for(size_t i = 0; i < g.size(); i++)
			if(type[i] == 0 && r[i] < 0.08 && x[i].abs() < 0.4)
			{
				deformation = true;
				break;
			}

		PSPrinter::Image ps;

		ps.setLineWidth(line_width);
		for(size_t i = 1; i < g.size(); i++)
			if(type[i] == 0)
			{
				arc(ps, x[i], x[i] + (x[ g[i] [2] ] - x[i]).normalized() * r[i], x[i] + (x[ g[i] [6] ] - x[i]).normalized() * r[i]);

				Vector2D c = cross(x[i], x[i] + (x[ g[i] [2] ] - x[i]).normalized() * r[i],
					x[i] + (x[ g[i] [6] ] - x[i]).normalized() * r[i],
					x[i] + (x[ g[i] [0] ] - x[i]).normalized() * r[i],
					x[i] + (x[ g[i] [4] ] - x[i]).normalized() * r[i]);

				ps.fill(Graph::Circle(c, cross_delta), Graph::Color::White);

				arc(ps, x[i], x[i] + (x[ g[i] [0] ] - x[i]).normalized() * r[i], x[i] + (x[ g[i] [4] ] - x[i]).normalized() * r[i]);
			}
			else if(type[i] == 1)
			{
				arc(ps, x[i], (g[i] [1] == 0) ? x[i].normalized() : x[i] + (x[ g[i] [1] ] - x[i]).normalized() * r[i],
					(g[i] [3] == 0) ? x[i].normalized() : x[i] + (x[ g[i] [3] ] - x[i]).normalized() * r[i]);
			}

		ps.setLineWidth(0.0);
		for(size_t i = 1; i < g.size(); i++)
		{
			if(type[i] != 1)
				continue;

			for(size_t j = 0; j < g[i].size(); j++)
				if(g[i] [j] == 0)
				{
					Vector2D p = x[i].normalized();
					ps.fill(Graph::Circle(p, end_radius));
				}
		}

		ps.setLineWidth(border_width);
		ps.setDash(dash_length * border_width, dash_length * border_width);
		ps.stroke(Graph::Circle(Vector2D(0.0, 0.0), 1.0));

		return ps.toString();
	}

private:
	double searchRad()
	{
		r.assign(g.size(), 0.1 / g.size());
		r[0] = 1.0;

		double time = clock();
		double error;
		size_t iteration = 0;
		do
		{
			for(int i = (int)g.size() - 1; i >= 0; i--)
			{
				std::vector<double> tmp = r;
				binarySearch(i);
				double angle = getAngle(i) - 2.0 * pi;

				if(fabs(angle) > 1e-5)
					r = tmp;
			}

			double lead = r[0];
			for(size_t i = 0; i < r.size(); i++)
				r[i] /= lead;

			error = 0.0;
			for(size_t i = 0; i < g.size(); i++)
			{
				double angle = getAngle(i) - 2.0 * pi;
				error += angle * angle;
			}
			error = sqrt(error);
			iteration++;
		} while(error > precision && iteration < 1000);

		time = (clock() - time) / CLOCKS_PER_SEC;
		std::cerr << "Found after " << iteration << " iterations; precision: " << error << " " << time << "s\n";
		return error;
	}

	void binarySearch(size_t i)
	{
		double u, d;

		if(getAngle(i) > 2.0 * pi)
			d = r[i], u = 2.0 * r[0];
		else
			d = 0.0, u = r[i];

		while(fabs(u - d) > 1e-9)
		{
			r[i] = 0.5 * (u + d);
			if(getAngle(i) > 2.0 * pi)
				d = r[i];
			else
				u = r[i];
		}
		r[i] = 0.5 * (u + d);
	}

	double getAngle(size_t u) const
	{
		double angle = 0.0;
		if(u == 0)
		{
			for(size_t i = 0; i < g[u].size(); i++)
			{
				size_t v = g[u] [i];
				size_t w = g[u] [ (i + 1) % g[u].size() ];

				double a = r[u] - r[v];
				double b = r[u] - r[w];
				double c = r[v] + r[w];

				if(a > 0.0 && b > 0.0 && c > 0.0 && c <= a + b && b <= a + c && a <= b + c)
					angle += acos((a * a + b * b - c * c) / (2 * a * b));
				else return 1e100;
			}
		}
		else
		{
			for(size_t i = 0; i < g[u].size(); i++)
			{
				size_t v = g[u] [i];
				size_t w = g[u] [ (i + 1) % g[u].size() ];
				if(w == 0)
					std::swap(v, w);

				if(v == 0)
				{
					double a = r[v] - r[u];
					double b = r[u] + r[w];
					double c = r[v] - r[w];

					if(a > 0.0 && b > 0.0 && c > 0.0 && c <= a + b && b <= a + c && a <= b + c)
						angle += acos(-(a * a + b * b - c * c) / (2 * a * b));
					else
						return -1e100;
				}
				else
				{
					double a = r[u] + r[v];
					double b = r[u] + r[w];
					double c = r[v] + r[w];
					angle += acos((a * a + b * b - c * c) / (2 * a * b));
				}
			}
		}

		return angle;
	}

	void getPositions()
	{
		x.assign(g.size(), Vector2D());
		std::vector<bool> ok(g.size(), false);
		ok[0] = true;

		for(size_t i = 1; i < g.size(); i++)
		{
			bool border = false;
			for(size_t j = 0; j < g[i].size(); j++)
				if(g[i] [j] == 0)
					border = true;

			if(border)
			{
				x[i] = Vector2D(1.0 - r[i], 0);
				ok[i] = true;
				break;
			}
		}

		while(true)
		{
			bool all = true;
			for(size_t i = 0; i < ok.size(); i++)
				if(!ok[i])
				{
					all = false;
					break;
				}

			if(all)
				break;

			for(size_t i = 0; i < g.size(); i++)
			{
				if(ok[i])
					continue;

				for(size_t j = 0; j < g[i].size(); j++)
				{
					size_t a = g[i] [j];
					size_t b = g[i] [ (j + 1) % g[i].size() ];

					if(!ok[a] || !ok[b])
						continue;

					x[i] = getCenter(i, a, b);
					ok[i] = true;
					break;
				}
			}
		}
	}

	Vector2D getCenter(size_t u, size_t v, size_t w) const
	{
		assert(u != 0 && (v != 0 || w != 0));

		if(v == 0)
			return getThirdVertex(x[w], x[v], r[w] + r[u], r[v] - r[u]);
		else if(w == 0)
			return getThirdVertex(x[w], x[v], r[w] - r[u], r[v] + r[u]);
		else
			return getThirdVertex(x[v], x[w], r[u] + r[v], r[u] + r[w]);
	}

	static Vector2D getThirdVertex(Vector2D u, Vector2D v, double a, double b)
	{
		double d = (u - v).abs();
		if(!(a + b >= d && a + d >= b && b + d >= a))
			return Vector2D(0, 0);
		double x = (d * d + a * a - b * b) / (2 * d);
		double y = sqrt(a * a - x * x);
		Vector2D xort = (v - u).normalized();
		return u + xort * x + xort.ort() * y;
	}

	static Vector2D trans(Vector2D p, Vector2D c)
	{
		Complex z(p.x, p.y), z0(c.x, c.y);
		Complex r = (z - z0) / (Complex(1.0) - z * (~z0));
		return Vector2D(r.re, r.im);
	}

	void move(Vector2D c)
	{
		for(size_t i = 1; i < g.size(); i++)
		{
			Vector2D p1 = x[i] + Vector2D(r[i], 0);
			Vector2D p2 = x[i] + Vector2D(-r[i], 0);
			Vector2D p3 = x[i] + Vector2D(0, r[i]);

			p1 = trans(p1, c);
			p2 = trans(p2, c);
			p3 = trans(p3, c);

			x[i] = Vector2D::getCircleCenter(p1, p2, p3);
			r[i] = (p1 - x[i]).abs();
		}
	}

	static std::pair<Vector2D, double> getOrtCenter(Vector2D c, Vector2D a, Vector2D b)
	{
		Vector2D center = ((a - c) + (b - c)).normalized();
		center = c + center * ((a - c).abs() / ((a - c).normalized() * center));
		return std::make_pair(center, (center - a).abs());
	}

	void arc(PSPrinter::Image & ps, Vector2D c, Vector2D a, Vector2D b) const
	{
		const size_t points = 10;

		if(fabs((a - c).normalized() * (b - c).normalized()) > 0.99)
		{
			if(deformation)
			{
				Graph::Chain chain;
				for(size_t i = 0; i <= points + 1; i++)
				{
					Vector2D p = a + (b - a) * i / (points + 1);
					p = p.normalized() * sqrt(p.abs());
					chain >> p;
				}
				ps.stroke(chain);
			}
			else
				ps.stroke(Graph::Line(a, b));
		}
		else
		{
			std::pair<Vector2D, double> tmp = getOrtCenter(c, a, b);

			Vector2D center = tmp.first;
			double r = tmp.second;

			double u = (a - center).atan2() / pi * 180.0;
			double d = (b - center).atan2() / pi * 180.0;

			if(d > u)
				std::swap(d, u);

			if(u - d >= 180.0)
			{
				d += 360.0;
				std::swap(d, u);
			}

			if(deformation)
			{
				Graph::Chain chain;
				for(size_t i = 0; i <= points + 1; i++)
				{
					Vector2D p = center + Vector2D::polar((d + (u - d) * i / (points + 1)) / 180.0 * pi) * r;
					p = p.normalized() * sqrt(p.abs());
					chain >> p;
				}
				ps.stroke(chain);
			}
			else
				ps.stroke(Graph::Arc(center, r, Geometry::Angle::fromDegrees(d), Geometry::Angle::fromDegrees(u)));
		}
	}

	Vector2D cross(Vector2D c, Vector2D a1, Vector2D b1, Vector2D a2, Vector2D b2) const
	{
		if(fabs((a1 - c).normalized() * (b1 - c).normalized()) > 0.99 && fabs((a2 - c).normalized() * (b2 - c).normalized()) > 0.99)
		{
			Vector2D p = Vector2D::intersection(a1, b1, a2, b2);
			if(deformation)
				return p.normalized() * sqrt(p.abs());
			else return p;
		}

		if(fabs((a1 - c).normalized() * (b1 - c).normalized()) <= 0.99 && fabs((a2 - c).normalized() * (b2 - c).normalized()) <= 0.99)
		{
			std::pair<Vector2D, double> tmp1 = getOrtCenter(c, a1, b1);
			std::pair<Vector2D, double> tmp2 = getOrtCenter(c, a2, b2);

			Vector2D c1 = tmp1.first, c2 = tmp2.first;
			double r1 = tmp1.second, r2 = tmp2.second, d = (c1 - c2).abs();

			double x = (d * d + r1 * r1 - r2 * r2) / (2 * d);
			double y = sqrt(r1 * r1 - x * x);

			Vector2D xort = (c2 - c1).normalized();
			Vector2D k1 = c1 + xort * x + xort.ort() * y;
			Vector2D k2 = c1 + xort * x - xort.ort() * y;

			Vector2D p;
			if((k1 - c).abs() <= (a1 - c).abs())
				p = k1;
			else
				p = k2;

			if(deformation)
				return p.normalized() * sqrt(p.abs());
			else
				return p;
		}

		if(fabs((a1 - c).normalized() * (b1 - c).normalized()) > 0.99)
		{
			std::swap(a1, a2);
			std::swap(b1, b2);
		}

		std::pair<Vector2D, double> tmp = getOrtCenter(c, a1, b1);
		Vector2D c1 = tmp.first;
		double r1 = tmp.second;

		Vector2D ort = (b2 - a2).normalized().ort();
		double x = (a2 - c1) * ort;
		double y = sqrt(r1 * r1 - x * x);

		Vector2D k1 = c1 + ort * x + ort.ort() * y;
		Vector2D k2 = c1 + ort * x - ort.ort() * y;

		Vector2D p;
		if((k1 - c).abs() <= (a1 - c).abs())
			p = k1;
		else
			p = k2;

		if(deformation)
			return p.normalized() * sqrt(p.abs());
		else
			return p;
	}

private:
	std::vector< std::vector<size_t> > g;
	std::vector<double> r;
	std::vector<Vector2D> x;
	std::vector<size_t> type;
	const double precision;
	bool deformation;
};
#endif

}}}}
