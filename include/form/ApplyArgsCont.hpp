#if ! defined(APPLYARGSCONT_HPP)
#define APPLYARGSCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ApplyArgsCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        ApplyArgsCont(ExprHandle op, Continuation * cont, Env * env);

        static ApplyArgsCont *
        makeInstance(ExprHandle op, Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run(ExprHandle expr) override;

    private:
        ExprHandle op;
        Continuation * cont;
        Env * env;
    };
}

#endif
