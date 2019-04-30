#if ! defined(ASSIGNCONT_HPP)
#define ASSIGNCONT_HPP 1

#include "form/EnvHelperCont.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class AssignCont : public EnvHelperCont
    {
    private:
        friend class scam::MemoryManager;

        AssignCont(ExprHandle sym,
                   Continuation * cont,
                   Env * env,
                   ScamEngine * engine);

        static AssignCont * makeInstance(ExprHandle sym,
                                         Continuation * cont,
                                         Env * env,
                                         ScamEngine * engine);

    protected:
        void finish(ExprHandle expr) const override;

    private:
        ScamEngine * engine;
    };
}

#endif
