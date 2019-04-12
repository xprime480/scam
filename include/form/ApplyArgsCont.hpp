#if ! defined(APPLYARGSCONT_HPP)
#define APPLYARGSCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class MemoryManager;
    class Env;
    class ScamExpr;

    class ApplyArgsCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ApplyArgsCont(ScamExpr * op, Continuation * cont, Env * env);

        static ApplyArgsCont *
        makeInstance(ScamExpr * op, Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run(ScamExpr * expr) override;

    private:
        ScamExpr * op;
        Continuation * cont;
        Env * env;
    };
}

#endif
