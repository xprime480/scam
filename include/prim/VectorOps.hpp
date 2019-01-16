#if ! defined(PRIMITIVE_VECTOROPS_H)
#define PRIMITIVE_VECTOROPS_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class VLen : public Primitive
    {
    public:
        VLen();

        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };

    class VRef : public Primitive
    {
    public:
        VRef();

        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };
}

#endif
