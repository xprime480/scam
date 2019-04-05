#if ! defined(PRIMITIVE_SYSTEMOPS_H)
#define PRIMITIVE_SYSTEMOPS_H 1

#include "prim/Primitive.hpp"

namespace scam
{
    class ScamEngine;

    class Load : public Primitive
    {
    private:
        Load(ScamEngine * engine);

    public:
        static Load * makeInstance(ScamEngine * engine);
        void applyArgs(ScamExpr * args, ContHandle cont) override;

    private:
        ScamEngine * engine;
    };

    class Spawn : public Primitive
    {
    private:
        Spawn();

    public:
        static Spawn * makeInstance();
        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };

    class Error : public Primitive
    {
    private:
        Error();

    public:
        static Error * makeInstance();
        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };

    class Backtrack : public Primitive
    {
    private:
        Backtrack(ScamEngine * engine);

    public:
        static Backtrack * makeInstance(ScamEngine * engine);
        void applyArgs(ScamExpr * args, ContHandle cont) override;

    private:
        ScamEngine * engine;
    };


    class Trace : public Primitive
    {
    private:
        Trace();

    public:
        static Trace * makeInstance();
        void applyArgs(ScamExpr * args, ContHandle cont) override;
    };


}

#endif
