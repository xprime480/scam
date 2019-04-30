#if ! defined(PRIMITIVE_TRACE_H)
#define PRIMITIVE_TRACE_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Continuation;

    class Trace : public Primitive
    {
    private:
        Trace();

    public:
        static Trace * makeInstance();
        void applyArgs(ExprHandle args, Continuation * cont) override;
    };
}

#endif
