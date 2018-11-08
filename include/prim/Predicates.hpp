#if ! defined(PRIMITIVE_PREDICATES_H)
#define PRIMITIVE_PREDICATES_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class NilP : public Primitive
    {
    public:
        NilP();

        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };
}

#endif
