#if ! defined(SCAMASSIGN_HPP)
#define SCAMASSIGN_HPP 1

#include "form/EnvHelper.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class Assign : public EnvHelper
    {
    private:
        friend class scam::MemoryManager;
        Assign(ScamEngine * engine);
        static Assign * makeInstance(ScamEngine * engine);

    public:
        void apply(ExprHandle args, Continuation * cont, Env * env) override;
    };
}

#endif
