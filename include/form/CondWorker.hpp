#if ! defined(CONDWORKER_HPP)
#define CONDWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class CondWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        CondWorker(ScamValue clauses, Continuation * cont, Env * env);

        static CondWorker *
        makeInstance(ScamValue clauses, Continuation * cont, Env * env);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue      clauses;
        Continuation * cont;
        Env          * env;

        bool checkForNullElse(ScamValue clauses);
        bool checkApplyForms(ScamValue clauses);
    };
}

#endif
