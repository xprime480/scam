#if ! defined(ORWORKER_HPP)
#define ORWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class OrWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        OrWorker(Continuation * cont, Env * env, ScamValue args);

        static OrWorker *
        makeInstance(Continuation * cont, Env * env, ScamValue args);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue      args;
        Continuation * cont;
        Env          * env;
    };
}

#endif
