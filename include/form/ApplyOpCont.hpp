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

        ApplyOpCont(ScamValue args, Continuation * cont, Env * env);

        static ApplyOpCont *
        makeInstance(ScamValue args, Continuation * cont, Env * env);

    public:
        void mark() const override;
        void run(ScamValue expr) override;

    private:
        ScamValue args;
        Continuation * cont;
        Env * env;
    };
}

#endif
