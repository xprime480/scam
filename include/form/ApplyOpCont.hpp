#if ! defined(APPLYOPCONT_HPP)
#define APPLYOPCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ApplyOpCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ApplyOpCont(ExprHandle args, Continuation * cont, Env * env);

        static ApplyOpCont *
        makeInstance(ExprHandle args, Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        ExprHandle args;
        Continuation * cont;
        Env * env;
    };
}

#endif
