#if ! defined(ASSIGNWORKER_HPP)
#define ASSIGNWORKER_HPP 1

#include "form/EnvHelperWorker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class AssignWorker : public EnvHelperWorker
    {
    private:
        friend class scam::MemoryManager;
        AssignWorker(ExprHandle args,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine);

        static AssignWorker * makeInstance(ExprHandle args,
                                           Continuation * cont,
                                           Env * env,
                                           ScamEngine * engine);

    protected:
        Continuation * getCont(ExprHandle sym) const override;

    private:
        ScamEngine * engine;
    };
}

#endif
