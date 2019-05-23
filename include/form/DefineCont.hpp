#if ! defined(DEFINECONT_HPP)
#define DEFINECONT_HPP 1

#include "form/EnvHelperCont.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class DefineCont : public EnvHelperCont
    {
    private:
        friend class scam::MemoryManager;

        DefineCont(ScamEnvKeyType sym,
                   Continuation * cont,
                   Env * env,
                   ScamEngine * engine);

        static DefineCont * makeInstance(ScamEnvKeyType sym,
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
