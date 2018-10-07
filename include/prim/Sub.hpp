#if ! defined(PRIMITIVE_SUB_H)
#define PRIMITIVE_SUB_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Sub : public Primitive
    {
    public:
        Sub();

        ExprHandle clone();

        void applyArgs(ExprHandle const & args, ContHandle cont) override;
    };
}

#endif
