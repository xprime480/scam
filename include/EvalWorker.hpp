#if ! defined(EVALWORKER_H)
#define EVALWORKER_H 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class EvalWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        EvalWorker(ScamValue forms, Env * env, Continuation * cont);

        static EvalWorker *
        makeInstance(ScamValue forms, Env * env, Continuation * cont);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue forms;
        Env * extended;
        Continuation * cont;
    };
}

#endif
