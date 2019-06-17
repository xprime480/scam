#if ! defined(MAPCDR_HPP)
#define MAPCDR_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"
#include "expr/WorkerData.hpp"

namespace scam
{
    class  MapCdr : public Worker
    {
    private:
        friend class scam::MemoryManager;

        MapCdr(ScamValue car,
               ScamValue cdr,
               Continuation * cont,
               Env * env,
               ScamEngine * engine);

        static MapCdr * makeInstance(ScamValue car,
                                     ScamValue cdr,
                                     Continuation * cont,
                                     Env * env,
                                     ScamEngine * engine);

    public:
        void mark() override;
        void run() override;

    private:
        WorkerData data;
    };
}

#endif
