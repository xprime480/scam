#if ! defined(IFWORKER_HPP)
#define IFWORKER_HPP 1

#include "ScamFwd.hpp"
#include "Worker.hpp"

namespace scam
{
    class Continuation;
    class Env;

    class IfWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        IfWorker(Continuation * cont, Env * env, ScamValue args);

        static IfWorker *
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
