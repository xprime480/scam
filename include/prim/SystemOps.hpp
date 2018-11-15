#if ! defined(PRIMITIVE_SYSTEMOPS_H)
#define PRIMITIVE_SYSTEMOPS_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Load : public Primitive
    {
    public:
        Load();

        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };
}

#endif
