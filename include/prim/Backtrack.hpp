#if ! defined(PRIMITIVE_BACKTRACK_H)
#define PRIMITIVE_BACKTRACK_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class ScamEngine;

    class Continuation;

    class Backtrack : public Primitive
    {
    private:
        Backtrack(ScamEngine * engine);

    public:
        /* I think this should be private, check later */
        static Backtrack * makeInstance(ScamEngine * engine);

        void applyArgs(ExprHandle args, Continuation * cont) override;

    private:
        ScamEngine * engine;
    };
}

#endif
