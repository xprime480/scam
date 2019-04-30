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

        DefineCont(ExprHandle sym,
                   Continuation * cont,
                   Env * env,
                   ScamEngine * engine);

        static DefineCont * makeInstance(ExprHandle sym,
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
