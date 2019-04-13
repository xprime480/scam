#if ! defined(WorkerData_HPP)
#define WorkerData_HPP 1

namespace scam
{
    class ScamExpr;
    class Continuation;
    class Env;

    struct WorkerData
    {
        WorkerData(ScamExpr * car,
                   ScamExpr * cdr,
                   Continuation * original,
                   Env * env);

        WorkerData(const WorkerData &) = default;
        WorkerData & operator=(const WorkerData &) = default;

        void mark() const;

        ScamExpr * car;
        ScamExpr * cdr;
        Continuation * original;
        Continuation * cont;
        Env * env;
    };
}

#endif
