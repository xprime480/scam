#if ! defined(DEFINEWORKER_HPP)
#define DEFINEWORKER_HPP 1

#include "form/EnvHelperWorker.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;
    class ScamEngine;

    class DefineWorker : public EnvHelperWorker
    {
    private:
        friend class scam::MemoryManager;
        DefineWorker(ScamExpr * args,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine);

        static DefineWorker * makeInstance(ScamExpr * args,
                                           Continuation * cont,
                                           Env * env,
                                           ScamEngine * engine);

    protected:
        Continuation * getCont(ScamExpr * sym) const override;

    private:
        ScamEngine * engine;
    };
}

#endif
