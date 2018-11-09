#if ! defined(PRIMITIVE_MISC_H)
#define PRIMITIVE_MISC_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Progn : public Primitive
    {
    public:
        Progn();

        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };
}

#endif
