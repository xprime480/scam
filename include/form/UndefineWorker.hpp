#if ! defined(UNDEFINEWORKER_HPP)
#define UNDEFINEWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class UndefineWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        UndefineWorker(ScamValue value, Continuation * cont, Env * env);

        static UndefineWorker *
        makeInstance(ScamValue value, Continuation * cont, Env * env);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue      value;
        Continuation * cont;
        Env          * env;
    };
}

#endif
