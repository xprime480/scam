#if ! defined(PRIMITIVE_VLEN_H)
#define PRIMITIVE_VLEN_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;

    class VLen : public Primitive
    {
    private:
        VLen();

    public:
        static VLen * makeInstance();
        void applyArgs(ScamExpr * args, Continuation * cont) override;
    };
}

#endif
