#if ! defined(ANDCONT_HPP)
#define ANDCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class AndCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        AndCont(ExprHandle args, Continuation * cont, Env * env, size_t n);

        static AndCont *
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
