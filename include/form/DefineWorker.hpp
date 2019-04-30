#if ! defined(DEFINEWORKER_HPP)
#define DEFINEWORKER_HPP 1

#include "form/EnvHelperWorker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class DefineWorker : public EnvHelperWorker
    {
    private:
        friend class scam::MemoryManager;
        DefineWorker(ExprHandle args,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine);

        static DefineWorker * makeInstance(ExprHandle args,
                                           Continuation * cont,
                                           Env * env,
                                           ScamEngine * engine);

    protected:
        Continuation * getCont(ScamEnvKeyType sym) const override;

    private:
        ScamEngine * engine;
    };
}

#endif
