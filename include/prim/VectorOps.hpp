#if ! defined(PRIMITIVE_VECTOROPS_H)
#define PRIMITIVE_VECTOROPS_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class VLen : public Primitive
    {
    private:
        VLen();

    public:
        static VLen * makeInstance();
        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };

    class VRef : public Primitive
    {
    private:
        VRef();

    public:
        static VRef * makeInstance();
        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };
}

#endif
