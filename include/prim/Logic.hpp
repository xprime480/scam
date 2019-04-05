#if ! defined(SCAMMATCH_H)
#define SCAMMATCH_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class ScamEngine;

    class Match : public Primitive
    {
    private:
        Match();

    public:
        static Match * makeInstance();
        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };

    class Unify : public Primitive
    {
    private:
        Unify();

    public:
        static Unify * makeInstance();
        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };

    class Substitute : public Primitive
    {
    private:
        Substitute();

    public:
        static Substitute * makeInstance();
        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };

    class Instantiate : public Primitive
    {
    private:
        Instantiate();

    public:
        static Instantiate * makeInstance();
        void applyArgs(ScamExpr * args, ContHandle cont) override;

    private:
        static size_t counter;
    };
}

#endif
