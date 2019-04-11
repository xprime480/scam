#if ! defined(PRIMITIVE_CARCDR_HPP)
#define PRIMITIVE_CARCDR_HPP 1

#include "prim/Primitive.hpp"

namespace scam
{
    class CarCdr : public Primitive
    {
    public:
        CarCdr(char const * name);

        void applyArgs(ScamExpr * args, Continuation * cont) override;

    protected:
        virtual void finish(ScamExpr * args, Continuation * cont) = 0;
    };
}

#endif
