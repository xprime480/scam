#if ! defined(ASSIGNWORKER_HPP)
#define ASSIGNWORKER_HPP 1

#include "form/EnvHelperWorker.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;
    class ScamEngine;

    class AssignWorker : public EnvHelperWorker
    {
    private:
        friend class scam::MemoryManager;
        AssignWorker(ScamExpr * args,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine);

        static AssignWorker * makeInstance(ScamExpr * args,
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
