#if ! defined(SCAMUNDEFINE_HPP)
#define SCAMUNDEFINE_HPP 1

#include "form/EnvHelper.hpp"

namespace scam
{
    class ScamEngine;
    class ScamExpr;
    class Continuation;
    class Env;
    class MemoryManager;


    class Undefine : public EnvHelper
    {
    private:
        friend class scam::MemoryManager;
        Undefine(ScamEngine * engine);
        static Undefine * makeInstance(ScamEngine * engine);

    public:
        void apply(ScamExpr * args,
                   Continuation * cont,
                   Env * env) override;
    };
}

#endif
