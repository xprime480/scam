#if ! defined(UNDEFINEWORKER_HPP)
#define UNDEFINEWORKER_HPP 1

#include "form/EnvHelperWorker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class UndefineWorker : public EnvHelperWorker
    {
    private:
        friend class scam::MemoryManager;
        UndefineWorker(ExprHandle args,
                       Continuation * cont,
                       Env * env,
                       ScamEngine * engine);

        static UndefineWorker * makeInstance(ExprHandle args,
                                             Continuation * cont,
                                             Env * env,
                                             ScamEngine * engine);

    protected:
        Continuation * getCont(ScamEnvKeyType sym) const override;
    };
}

#endif
