#if ! defined(PRIMITIVE_ERROR_H)
#define PRIMITIVE_ERROR_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Continuation;

    class Error : public Primitive
    {
    private:
        Error();

    public:
        static Error * makeInstance();
        void applyArgs(ExprHandle args, Continuation * cont) override;
    };
}

#endif
