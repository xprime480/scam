#if ! defined(SCAMENVHELPER_H)
#define SCAMENVHELPER_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class ScamEngine;

    class EnvHelper : public SpecialForm
    {
    public:
        EnvHelper(char const * name, ScamEngine * engine);

    protected:
        ScamEngine * engine;
    };

    class Define : public EnvHelper
    {
    private:
        Define(ScamEngine * engine);

    public:
        static Define * makeInstance(ScamEngine * engine);
        void apply(ScamExpr * args, Continuation * cont, Env * env) override;
    };

    class Undefine : public EnvHelper
    {
    private:
        Undefine(ScamEngine * engine);

    public:
        static Undefine * makeInstance(ScamEngine * engine);
        void apply(ScamExpr * args, Continuation * cont, Env * env) override;
    };

    class Assign : public EnvHelper
    {
    private:
        Assign(ScamEngine * engine);

    public:
        static Assign * makeInstance(ScamEngine * engine);
        void apply(ScamExpr * args, Continuation * cont, Env * env) override;
    };
}

#endif
