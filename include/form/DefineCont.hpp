#if ! defined(DEFINECONT_HPP)
#define DEFINECONT_HPP 1

#include "form/EnvHelperCont.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;
    class ScamEngine;
    class MemoryManager;

    class DefineCont : public EnvHelperCont
    {
    private:
        friend class scam::MemoryManager;

        DefineCont(ScamExpr * sym,
                   Continuation * cont,
                   Env * env,
                   ScamEngine * engine);

        static DefineCont * makeInstance(ScamExpr * sym,
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
