#pragma once

#ifdef __cplusplus
namespace Math { namespace Manifolds { namespace Embedding { namespace Optimization { namespace Relax {
#endif

	typedef struct
	{
		const double border;
		const double electric;
		const double bend;
		const double elastic;
		const double cross;
	} InteractionConst;

#ifdef __cplusplus
}}}}}
#endif
