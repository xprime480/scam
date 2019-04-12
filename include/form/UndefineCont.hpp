#if ! defined(UNDEFINECONT_HPP)
#define UNDEFINECONT_HPP 1

#include "form/EnvHelperCont.hpp"

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;
    class MemoryManager;

    class UndefineCont : public EnvHelperCont
    {
    private:
        friend class scam::MemoryManager;

        UndefineCont(ScamExpr * sym, Continuation * cont, Env * env);

        static UndefineCont *
        makeInstance(ScamExpr * sym, Continuation * cont, Env * env);

    protected:
        void finish(ScamExpr * expr) const override;
    };
}

#endif
