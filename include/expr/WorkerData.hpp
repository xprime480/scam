#if ! defined(WORKERDATA_HPP)
#define WORKERDATA_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    struct WorkerData
    {
        WorkerData(ScamValue car,
                   ScamValue cdr,
                   Continuation * original,
                   Env * env);

        WorkerData(const WorkerData &) = default;
        WorkerData & operator=(const WorkerData &) = default;

        void mark() const;

        ScamValue car;
        ScamValue cdr;
        Continuation * original;
        Continuation * cont;
        Env * env;
    };
}

#endif
