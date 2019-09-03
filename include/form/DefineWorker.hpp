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
                     Env * env);

        static DefineWorker * makeInstance(ScamValue symbol,
                                           ScamValue form,
                                           Continuation * cont,
                                           Env * env);

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
