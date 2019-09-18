#if ! defined(CONDTESTCONT_HPP)
#define CONDTESTCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class CondTestCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        CondTestCont(ScamValue forms,
                     ScamValue clauses,
                     Continuation * cont,
                     Env * env);

        static CondTestCont * makeInstance(ScamValue forms,
                                           ScamValue clauses,
                                           Continuation * cont,
                                           Env * env);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        ScamValue      forms;
        ScamValue      clauses;
        Continuation * cont;
        Env          * env;
    };
}

#endif
