#if ! defined(ASSIGNCONT_HPP)
#define ASSIGNCONT_HPP 1

#include "form/EnvHelperCont.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;
    class ScamEngine;

    class AssignCont : public EnvHelperCont
    {
    private:
        friend class scam::MemoryManager;

        AssignCont(ScamExpr * sym,
                   Continuation * cont,
                   Env * env,
                   ScamEngine * engine);

        static AssignCont * makeInstance(ScamExpr * sym,
                                         Continuation * cont,
                                         Env * env,
                                         ScamEngine * engine);

    protected:
        void finish(ScamExpr * expr) const override;

    private:
        ScamEngine * engine;
    };
}

#endif
