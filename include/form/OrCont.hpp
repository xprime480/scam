#if ! defined(ORCONT_HPP)
#define ORCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class MemoryManager;
    class Env;
    class ScamExpr;
    
    class OrCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        OrCont(ScamExpr * args, Continuation * cont, Env * env, size_t n);

        static OrCont *
        makeInstance(ScamExpr * args, Continuation * cont, Env * env, size_t n);

    public:
        void mark() const override;

        void run(ScamExpr * expr) override;

    private:
        ScamExpr * args;
        Continuation * cont;
        Env * env;
        size_t n;
    };
}

#endif
