#if ! defined(PRIMITIVE_SYSTEMOPS_H)
#define PRIMITIVE_SYSTEMOPS_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class ScamEngine;

    class Load : public Primitive
    {
    public:
        Load(ScamEngine * engine);

        void applyArgs(ScamExpr * args, ContHandle cont) override;

    private:
        ScamEngine * engine;
    };

    class Spawn : public Primitive
    {
    public:
        Spawn();

        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };
}

#endif
