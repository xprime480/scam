#if ! defined(CONSWORKER_HPP)
#define CONSWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class ConsWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        ConsWorker(ScamValue car,
                   ScamValue cdr,
                   Continuation * original,
                   Env * env,
                   ScamEngine * engine);

        static ConsWorker * makeInstance(ScamValue car,
                                         ScamValue cdr,
                                         Continuation * original,
                                         Env * env,
                                         ScamEngine * engine);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue car;
        Continuation * cont;
        Env * env;
    };
}

#endif
