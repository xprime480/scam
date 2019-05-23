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
                   ScamValue cdr);

        ConsWorker(WorkerData const & data);

        static ConsWorker * makeInstance(Continuation * cont,
                                         Env * env,
                                         ScamValue car,
                                         ScamValue cdr);

        static ConsWorker * makeInstance(WorkerData const & data);

    public:
        void mark() const override;
        void run() override;

    private:
        WorkerData data;
    };
}

#endif
