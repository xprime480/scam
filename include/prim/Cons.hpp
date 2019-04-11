#if ! defined(PRIMITIVE_CONS_HPP)
#define PRIMITIVE_CONS_HPP 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Cons : public Primitive
    {
    private:
        Cons();

    public:
        static Cons * makeInstance();
        void applyArgs(ScamExpr * args, Continuation * cont) override;
    };
}

#endif
