#if ! defined(IFCONT_HPP)
#define IFCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class MemoryManager;
    class ScamExpr;
    class Env;

    class IfCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;
        IfCont(ScamExpr * args, Continuation * cont, Env * env);

        static IfCont *
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
