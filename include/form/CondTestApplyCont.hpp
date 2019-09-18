#if ! defined(CONDTESTAPPLYCONT_HPP)
#define CONDTESTAPPLYCONT_HPP 1

#include "Continuation.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class CondTestApplyCont : public Continuation
    {
    private:
        friend class scam::MemoryManager;

        CondTestApplyCont(ScamValue form,
                          ScamValue clauses,
                          Continuation * cont,
                          Env * env);

        static CondTestApplyCont * makeInstance(ScamValue form,
                                                ScamValue clauses,
                                                Continuation * cont,
                                                Env * env);

    public:
        void mark() override;
        void handleValue(ScamValue value) override;

    private:
        ScamValue      form;
        ScamValue      clauses;
        Continuation * cont;
        Env          * env;
    };
}

#endif
