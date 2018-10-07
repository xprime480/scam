#if ! defined(PRIMITIVE_MUL_H)
#define PRIMITIVE_MUL_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Mul : public Primitive
    {
    public:
        Mul();

        ExprHandle clone();

        void applyArgs(ExprHandle const & args, ContHandle cont) override;
    };
}

#endif
