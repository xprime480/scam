#if ! defined(SCAMASSIGN_HPP)
#define SCAMASSIGN_HPP 1

#include "form/EnvHelper.hpp"

namespace scam
{
    class ScamEngine;
    class ScamExpr;
    class Continuation;
    class Env;
    class MemoryManager;

    class Assign : public EnvHelper
    {
    private:
        friend class scam::MemoryManager;
        Assign(ScamEngine * engine);
        static Assign * makeInstance(ScamEngine * engine);

    public:
        void apply(ScamExpr * args,
                   Continuation * cont,
                   Env * env) override;
    };
}

#endif
