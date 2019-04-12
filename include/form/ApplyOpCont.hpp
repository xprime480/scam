#if ! defined(APPLYOPCONT_HPP)
#define APPLYOPCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class MemoryManager;
    class Env;
    class ScamExpr;

    class ApplyOpCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ApplyOpCont(ScamExpr * args, Continuation * cont, Env * env);

        static ApplyOpCont *
        makeInstance(ScamExpr * args, Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        ScamExpr * args;
        Continuation * cont;
        Env * env;
    };
}

#endif
