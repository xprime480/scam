#if ! defined(PRIMITIVE_BACKTRACK_H)
#define PRIMITIVE_BACKTRACK_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class ScamEngine;
    class ScamExpr;
    class Continuation;

    class Backtrack : public Primitive
    {
    private:
        Backtrack(ScamEngine * engine);

    public:
        static Backtrack * makeInstance(ScamEngine * engine);
        void applyArgs(ScamExpr * args, Continuation * cont) override;

    private:
        ScamEngine * engine;
    };
}

#endif
