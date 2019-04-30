#if ! defined(SCAMDEFINE_HPP)
#define SCAMDEFINE_HPP 1

#include "form/EnvHelper.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class Define : public EnvHelper
    {
    private:
        friend class scam::MemoryManager;
        Define(ScamEngine * engine);
        static Define * makeInstance(ScamEngine * engine);

    public:
        void
        apply(ExprHandle args, Continuation * cont, Env * env) override;
    };
}

#endif
