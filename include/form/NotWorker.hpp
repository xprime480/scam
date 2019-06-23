#if ! defined(NOTWORKER_HPP)
#define NOTWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class NotWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        NotWorker(Continuation * cont,
                  Env * env,
                  ScamEngine * engine,
                  ScamValue value);

        static NotWorker * makeInstance(Continuation * cont,
                                        Env * env,
                                        ScamEngine * engine,
                                        ScamValue value);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue      value;
        Continuation * cont;
        Env          * env;
    };
}

#endif
