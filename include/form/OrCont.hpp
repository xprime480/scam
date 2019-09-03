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

        OrCont(ScamValue args, Continuation * cont, Env * env);

        static OrCont *
        makeInstance(ScamValue args, Continuation * cont, Env * env);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        ScamValue args;
        Continuation * cont;
        Env * env;
    };
}

#endif
