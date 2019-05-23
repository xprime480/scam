#if ! defined(PRIMITIVE_SPAWN_H)
#define PRIMITIVE_SPAWN_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class Continuation;

    class Spawn : public Primitive
    {
    private:
        Spawn();

    public:
        static Spawn * makeInstance();
        void applyArgs(ScamValue args, Continuation * cont) override;
    };
}

#endif
