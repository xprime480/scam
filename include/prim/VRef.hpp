#if ! defined(PRIMITIVE_VREF_H)
#define PRIMITIVE_VREF_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Continuation;

    class VRef : public Primitive
    {
    private:
        VRef();

    public:
        static VRef * makeInstance();
        void applyArgs(ExprHandle args, Continuation * cont) override;
    };
}

#endif
