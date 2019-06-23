#if ! defined(DEFINEWORKER_HPP)
#define DEFINEWORKER_HPP 1

#include "Worker.hpp"

#include "ScamFwd.hpp"

namespace scam
{
    class DefineWorker : public Worker
    {
    private:
        friend class scam::MemoryManager;

        DefineWorker(ScamValue symbol,
                     ScamValue form,
                     Continuation * cont,
                     Env * env,
                     ScamEngine * engine);

        static DefineWorker * makeInstance(ScamValue symbol,
                                           ScamValue form,
                                           Continuation * cont,
                                           Env * env,
                                           ScamEngine * engine);

    public:
        void mark() override;
        void run() override;

    private:
        ScamValue      symbol;
        ScamValue      form;
        Continuation * cont;
        Env          * env;
    };
}

#endif
