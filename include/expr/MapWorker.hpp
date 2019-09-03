#if ! defined(MAPWORKER_HPP)
#define MAPWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class  MapWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        MapWorker(ScamValue car,
                  ScamValue cdr,
                  Continuation * original,
                  Env * env);

        static MapWorker *
        makeInstance(ScamValue car,
                     ScamValue cdr,
                     Continuation * original,
                     Env * env);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue      car;
        Continuation * cont;
        Env          * env;
    };
}

#endif
