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

        AssignCont(ScamValue sym,
                   Continuation * cont,
                   Env * env,
                   ScamEngine * engine);

        static AssignCont * makeInstance(ScamValue sym,
                                         Continuation * cont,
                                         Env * env,
                                         ScamEngine * engine);

    protected:
        void finish(ScamValue expr) const override;

    private:
        ScamEngine * engine;
    };
}

#endif
