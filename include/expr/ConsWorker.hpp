#if ! defined(CONSWORKER_HPP)
#define CONSWORKER_HPP 1

#include "Worker.hpp"

#include "expr/WorkerData.hpp"

namespace scam
{
    class Congtinuation;
    class Env;
    class ScamExpr;
    class MemoryManager;

    class  ConsWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        ConsWorker(Continuation * cont,
                   Env * env,
                   ScamExpr * car,
                   ScamExpr * cdr);

        ConsWorker(WorkerData const & data);

        static ConsWorker * makeInstance(Continuation * cont,
                                         Env * env,
                                         ScamExpr * car,
                                         ScamExpr * cdr);
        static ConsWorker * makeInstance(WorkerData const & data);

    public:
        void mark() const override;
        void run() override;

    private:
        WorkerData data;
    };
}

#endif
