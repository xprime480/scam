#if ! defined(PRIMWORKER_HPP)
#define PRIMWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class  PrimWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;
        PrimWorker(ScamValue caller,
                   ScamValue args,
                   Continuation * original,
                   Env * env);

        static PrimWorker * makeInstance(ScamValue caller,
                                         ScamValue args,
                                         Continuation * original,
                                         Env * env);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue      args;
        Continuation * cont;
        Env          * env;
    };
}

#endif
