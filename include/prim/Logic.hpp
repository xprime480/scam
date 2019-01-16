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

    class Substitute : public Primitive
    {
    public:
        Substitute();

        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };

    class Instantiate : public Primitive
    {
    public:
        Instantiate();

        void applyArgs(ScamExpr * args, ContHandle cont) override;

    private:
	static size_t counter;
    };
}

#endif
