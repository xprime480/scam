#if ! defined(ANDWORKER_HPP)
#define ANDWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class AndWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        AndWorker(Continuation * cont, Env * env, ScamValue args);

        static AndWorker *
        makeInstance(Continuation * cont, Env * env, ScamValue args);

    public:
        void mark() override;
        void run() override;

    private:
        Continuation * cont;
        Env          * env;
        ScamValue      args;
    };
}

#endif
