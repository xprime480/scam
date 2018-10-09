#if ! defined(PRIMITIVE_DIV_H)
#define PRIMITIVE_DIV_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Div : public Primitive
    {
    public:
        Div();

        ExprHandle clone() const;

        void applyArgs(ExprHandle const & args, ContHandle cont) override;
    };
}

#endif
