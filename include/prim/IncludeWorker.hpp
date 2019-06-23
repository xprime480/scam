#if ! defined(INCLUDEWORKER_HPP)
#define INCLUDEWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class IncludeWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        IncludeWorker(ScamValue args, Continuation * cont, ScamEngine * engine);

        static IncludeWorker *
        makeInstance(ScamValue args, Continuation * cont, ScamEngine * engine);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue args;
        Continuation * cont;
    };
}

#endif
