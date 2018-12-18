#if ! defined(PRIMITIVE_EQUALP_H)
#define PRIMITIVE_EQUALP_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class EqualP : public Primitive
    {
    public:
        EqualP();

        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };
}

#endif