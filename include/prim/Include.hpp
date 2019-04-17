#if ! defined(PRIMITIVE_INCLUDE_H)
#define PRIMITIVE_INCLUDE_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class ScamEngine;
    class ScamExpr;
    class Continuation;

    class Include : public Primitive
    {
    private:
        Include(ScamEngine * engine);

    public:
        static Include * makeInstance(ScamEngine * engine);
        void applyArgs(ScamExpr * args, Continuation * cont) override;

    private:
        ScamEngine * engine;
    };
}

#endif
