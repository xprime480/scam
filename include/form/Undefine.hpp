#if ! defined(SCAMUNDEFINE_HPP)
#define SCAMUNDEFINE_HPP 1

#include "form/EnvHelper.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class Undefine : public EnvHelper
    {
    private:
        friend class scam::MemoryManager;
        Undefine(ScamEngine * engine);
        static Undefine * makeInstance(ScamEngine * engine);

    public:
        void apply(ExprHandle args, Continuation * cont, Env * env) override;
    };
}

#endif
