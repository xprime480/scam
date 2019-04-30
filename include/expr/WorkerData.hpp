#if ! defined(WORKERDATA_HPP)
#define WORKERDATA_HPP 1

#include "ScamFwd.hpp"

namespace scam
{
    struct WorkerData
    {
        WorkerData(ExprHandle car,
                   ExprHandle cdr,
                   Continuation * original,
                   Env * env);

        WorkerData(const WorkerData &) = default;
        WorkerData & operator=(const WorkerData &) = default;

        void mark() const;

        ExprHandle car;
        ExprHandle cdr;
        Continuation * original;
        Continuation * cont;
        Env * env;
    };
}

#endif
