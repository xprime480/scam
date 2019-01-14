#if ! defined(SCAMMATCH_H)
#define SCAMMATCH_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class ScamEngine;

    class Match : public Primitive
    {
    public:
        Match();

        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };

    class Unify : public Primitive
    {
    public:
        Unify();

        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };
}

#endif
