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
    public:
        Define(ScamEngine * engine);

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };

    class Undefine : public EnvHelper
    {
    public:
        Undefine(ScamEngine * engine);

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };

    class Assign : public EnvHelper
    {
    public:
        Assign(ScamEngine * engine);

        void apply(ScamExpr * args, ContHandle cont, Env env) override;
    };
}

#endif
