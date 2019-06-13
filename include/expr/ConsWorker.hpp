#if ! defined(CONSWORKER_HPP)
#define CONSWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"
#include "expr/WorkerData.hpp"

namespace scam
{
    class  ConsWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        ConsWorker(Continuation * cont,
                   Env * env,
                   ScamValue car,
                   ScamValue cdr,
                   ScamEngine * engine);

        ConsWorker(WorkerData const & data, ScamEngine * engine);

        static ConsWorker * makeInstance(Continuation * cont,
                                         Env * env,
                                         ScamValue car,
                                         ScamValue cdr,
                                         ScamEngine * engine);

        static ConsWorker *
        makeInstance(WorkerData const & data, ScamEngine * engine);

    public:
        void mark() const override;
        void run() override;

    private:
        WorkerData data;
    };
}

#endif
