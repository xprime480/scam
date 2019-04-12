#if ! defined(SCAMDEFINE_HPP)
#define SCAMDEFINE_HPP 1

#include "form/EnvHelper.hpp"

namespace scam
{
    class ScamEngine;
    class ScamExpr;
    class Continuation;
    class Env;
    class MemoryManager;

    class Define : public EnvHelper
    {
    private:
        friend class scam::MemoryManager;
        Define(ScamEngine * engine);
        static Define * makeInstance(ScamEngine * engine);

    public:
        void apply(ScamExpr * args,
                   Continuation * cont,
                   Env * env) override;
    };
}

#endif
