#if ! defined(PRIMITIVE_ADD_H)
#define PRIMITIVE_ADD_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Add : public Primitive
    {
    public:
        Add();

        void applyArgs(ExprHandle const & args, ContHandle cont) override;
    };
}

#endif
