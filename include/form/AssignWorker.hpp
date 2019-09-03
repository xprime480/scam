#if ! defined(ASSIGNWORKER_HPP)
#define ASSIGNWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class AssignWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        AssignWorker(ScamValue sym,
                     ScamValue value,
                     Continuation * cont,
                     Env * env);

        static AssignWorker * makeInstance(ScamValue sym,
                                           ScamValue value,
                                           Continuation * cont,
                                           Env * env);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue      sym;
        ScamValue      value;
        Continuation * cont;
        Env          * env;
    };
}

#endif
