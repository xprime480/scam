#if ! defined(UNDEFINEWORKER_HPP)
#define UNDEFINEWORKER_HPP 1

#include "form/EnvHelperWorker.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;
    class ScamEngine;

    class UndefineWorker : public EnvHelperWorker
    {
    private:
        friend class scam::MemoryManager;
        UndefineWorker(ScamExpr * args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine);

        static UndefineWorker * makeInstance(ScamExpr * args,
                                             Continuation * cont,
                                             Env * env,
                                             ScamEngine * engine);

    protected:
        Continuation * getCont(ScamExpr * sym) const override;
    };
}

#endif
