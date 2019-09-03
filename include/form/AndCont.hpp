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

        AndCont(ScamValue args, Continuation * cont, Env * env);

        static AndCont *
        makeInstance(ScamValue args, Continuation * cont, Env * env);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        ScamValue      args;
        Continuation * cont;
        Env          * env;
    };
}

#endif
