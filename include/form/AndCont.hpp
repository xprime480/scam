#if ! defined(ANDCONT_HPP)
#define ANDCONT_HPP 1

#include "Continuation.hpp"

namespace scam
{
    class MemoryManager;
    class Continuation;
    class Env;
    class ScamExpr;

    class AndCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        AndCont(ScamExpr * args, Continuation * cont, Env * env, size_t n);

        static AndCont *
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
