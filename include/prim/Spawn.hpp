#if ! defined(PRIMITIVE_SPAWN_H)
#define PRIMITIVE_SPAWN_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;

    class Spawn : public Primitive
    {
    private:
        Spawn();

    public:
        static Spawn * makeInstance();
        void applyArgs(ScamExpr * args, Continuation * cont) override;
    };
}

#endif
