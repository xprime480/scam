#if ! defined(SCAMENVHELPER_H)
#define SCAMENVHELPER_H 1

#include "form/SpecialForm.hpp"

namespace scam
{
    class ScamEngine;
    class ScamExpr;
    class Continuation;

    class EnvHelper : public SpecialForm
    {
    public:
        EnvHelper(char const * name, ScamEngine * engine);

    protected:
        ScamEngine * engine;

        bool checkArgs(ScamExpr * args,
                       Continuation * cont,
                       bool exprNeeded);
    };
}

#endif
