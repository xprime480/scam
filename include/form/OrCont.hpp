#if ! defined(ORCONT_HPP)
#define ORCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class OrCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        OrCont(ExprHandle args, Continuation * cont, Env * env, size_t n);

        static OrCont *
        makeInstance(ExprHandle args, Continuation * cont, Env * env, size_t n);

    public:
        void mark() const override;

        void run(ExprHandle expr) override;

    private:
        ExprHandle args;
        Continuation * cont;
        Env * env;
        size_t n;
    };
}

#endif
