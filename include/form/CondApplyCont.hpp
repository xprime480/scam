#if ! defined(CONDAPPLYCONT_HPP)
#define CONDAPPLYCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class CondApplyCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        CondApplyCont(ScamValue arg, Continuation * cont, Env * env);

        static CondApplyCont *
        makeInstance(ScamValue arg, Continuation * cont, Env * env);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        ScamValue      arg;
        Continuation * cont;
        Env          * env;
    };
}

#endif
